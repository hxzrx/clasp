
#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <unistd.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/array.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/primitives.h> // cl__fdefinition

namespace core {


void BytecodeModule_O::initialize() {
  this->_Literals = ComplexVector_T_O::make(16);
  this->_Bytecode = ComplexVector_byte8_t_O::make(1024,0,true,core::make_fixnum(0),nil<core::T_O>(),false, core::make_fixnum(0));
}

CL_DEFMETHOD
BytecodeModule_O::Literals_sp_Type BytecodeModule_O::literals() const {
  return this->_Literals;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_literals(BytecodeModule_O::Literals_sp_Type o) {
  this->_Literals = o;
}

CL_DEFMETHOD
BytecodeModule_O::Bytecode_sp_Type BytecodeModule_O::bytecode() const {
  return this->_Bytecode;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_bytecode(BytecodeModule_O::Bytecode_sp_Type o) {
  this->_Bytecode = o;
}

static void write_uint16(ComplexVector_byte8_t_sp buffer, unsigned int value ) {
  if ( value <= 127 ) {
    buffer->vectorPushExtend(value);
  } else {
    unsigned byte0 = 0x80 | (value&0x7f);
    unsigned byte1 = (value>>7) & 0xff;
    buffer->vectorPushExtend(byte0);
    buffer->vectorPushExtend(byte1);
  }
}

static inline uint16_t read_uint16( unsigned char*& pc ) {
  uint16_t byte0 = (uint16_t)(*(++pc));
  if (byte0<=127) return byte0;
  uint16_t byte1 = (uint16_t)(*(++pc));
  uint16_t result = ((uint16_t)byte1)<<7 | ((uint16_t)byte0)&0x7f;
  return result;
}

CL_DEFUN void core__write_uint16( ComplexVector_byte8_t_sp buffer, uint16_t value)
{
  write_uint16(buffer,value);
}

CL_DEFUN Fixnum core__read_uint16( ComplexVector_byte8_t_sp buffer, size_t index )
{
  unsigned char* pc = (unsigned char*)(buffer->rowMajorAddressOfElement_(index))+index;
  return (Fixnum)read_uint16(pc);
}

static void write_int32(ComplexVector_byte8_t_sp buffer, int value ) {
  if ( value >= -64 && value <= 63 ) {
    buffer->vectorPushExtend(value&0x7f);
    return;
  } else if (value >= -16384 && value <= 16383 ) {
    buffer->vectorPushExtend((value&0x7f)|0x80);
    buffer->vectorPushExtend((value>>7)&0xff);
    return;
  }
  buffer->vectorPushExtend(0x80);
  buffer->vectorPushExtend(0);
  buffer->vectorPushExtend(value&0xff);
  buffer->vectorPushExtend((value>>8)&0xff);
  buffer->vectorPushExtend((value>>16)&0xff);
  buffer->vectorPushExtend((value>>24)&0xff);
}

static inline int32_t read_int32(unsigned char*& pc) {
  unsigned char byte0 = *(++pc);
  if (byte0<=127) {
    if ((byte0&0x40)==0) { // positive
      return (int32_t)(byte0&0x3f);
    }
    return (int32_t)(0xFFFFff80 | byte0);
  }
  unsigned char byte1 = *(++pc);
  if ((byte1&0x80)==0) { // positive
    int result = ((int32_t)byte1)<<7|((int32_t)(byte0&0x7f));
    if (result == 0) {
      uint32_t ibyte0 = *(++pc);
      uint32_t ibyte1 = *(++pc);
      uint32_t ibyte2 = *(++pc);
      uint32_t ibyte3 = *(++pc);
      return (ibyte3<<24)|(ibyte2<<16)|(ibyte1<<8)|ibyte0;
    }
    return result;
  }
  return (int32_t)(0xFFFF8000 | ((int32_t)byte1<<7) | ((int32_t)(byte0&0x7f)));
}

CL_DEFUN void core__write_int32(ComplexVector_byte8_t_sp buffer, int value) {
  write_int32(buffer,value);
}

CL_DEFUN int core__read_int32(ComplexVector_byte8_t_sp buffer, size_t index) {
  unsigned char* pc = (unsigned char*)(buffer->rowMajorAddressOfElement_(index))+index;
  return read_int32(pc);
}

static inline int32_t read_label(unsigned char*& pc, size_t nbytes) {
  // Labels are stored little-endian.
  uint32_t result = 0;
  for (size_t i = 0; i < nbytes - 1; ++i) result |= *(++pc) << i * 8;
  uint8_t msb = *(++pc);
  result |= (msb & 0x7f) << ((nbytes - 1) * 8);
  // Signed conversion. TODO: Get something that optimizes well.
  if (msb & 0x80)
    return static_cast<int32_t>(result) - (1 << (8 * nbytes - 1));
  return static_cast<int32_t>(result);
}

gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args)
{
  ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
  // Do we need the lookup entryPoint? - if so - maybe we should pass it to bytecode_call
  // Meh, it's just a lookup and we are going to read closed over slots
  // which should be in the same cache line.
  core::GlobalBytecodeEntryPoint_sp entryPoint = gctools::As_unsafe<core::GlobalBytecodeEntryPoint_sp>(closure->_EntryPoint.load());
  printf("%s:%d:%s This is where we evaluate bytecode functions pc: %p\n", __FILE__, __LINE__, __FUNCTION__, pc );
  size_t nlocals = entryPoint->localsFrameSize();
  BytecodeModule_sp module = entryPoint->code();
  SimpleVector_sp literals = gc::As<SimpleVector_sp>(module->literals());

  VirtualMachine& vm = my_thread->_VM;
  vm.push_frame(nlocals);
  while (1) {
    switch (*pc) {
    case vm_ref: {
      uint16_t n = read_uint16(pc);
      printf("ref %" PRIu16 "\n", n);
      vm.push(*(vm.reg(n)));
      pc++;
      break;
    }
    case vm_const: {
      uint16_t n = read_uint16(pc);
      printf("const %" PRIu16 "\n", n);
      vm.push(literals->rowMajorAref(n).raw_());
      pc++;
      break;
    }
    case vm_closure: {
      uint16_t n = read_uint16(pc);
      printf("closure %" PRIu16 "\n", n);
      vm.push((*closure)[n].raw_());
      pc++;
      break;
    }
    case vm_call: {
      uint16_t nargs = read_uint16(pc);
      printf("call %" PRIu16 "\n", nargs);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      res.saveToMultipleValue0();
      vm.drop(nargs+1);
      pc++;
      break;
    }
    case vm_call_receive_one: {
      uint16_t nargs = read_uint16(pc);
      printf("call-receive-one %" PRIu16 "\n", nargs);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.drop(nargs+1);
      vm.push(res.raw_());
      pc++;
      break;
    }
    case vm_call_receive_fixed: {
      uint16_t nargs = read_uint16(pc);
      uint16_t nvals = read_uint16(pc);
      printf("call-receive-fixed %" PRIu16 " %" PRIu16 "\n", nargs, nvals);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.drop(nargs+1);
      if (nvals != 0) {
        MultipleValues &mv = lisp_multipleValues();
        vm.push(res.raw_()); // primary
        size_t svalues = mv.getSize();
        for (size_t i = 1; i < svalues; ++i)
          vm.push(mv.valueGet(i, svalues).raw_());
      }
      pc++;
      break;
    }
    case vm_bind: {
      uint16_t nelems = read_uint16(pc);
      uint16_t base = read_uint16(pc);
      printf("bind %" PRIu16 " %" PRIu16 "\n", nelems, base);
      vm.copytoreg(vm.stackref(nelems-1), nelems, base);
      vm.drop(nelems);
      pc++;
      break;
    }
    case vm_set: {
      uint16_t n = read_uint16(pc);
      printf("set %" PRIu16 "\n", n);
      vm.copytoreg(vm.stackref(0), 1, n);
      vm.drop(1);
      pc++;
      break;
    }
    case vm_make_cell: {
      printf("make-cell\n");
      T_sp car((gctools::Tagged)(vm.pop()));
      T_sp cdr((gctools::Tagged)nil<T_O*>);
      vm.push(Cons_O::create(car, cdr).raw_());
      pc++;
      break;
    }
    case vm_cell_ref: {
      printf("cell-ref\n");
      T_sp cons((gctools::Tagged)vm.pop());
      vm.push(oCar(cons).raw_());
      pc++;
      break;
    }
    case vm_cell_set: {
      printf("cell-set\n");
      T_O* val = vm.pop();
      T_sp tval((gctools::Tagged)val);
      T_sp cons((gctools::Tagged)vm.pop());
      CONS_CAR(cons) = tval;
      pc++;
      break;
    }
    case vm_make_closure: {
      uint16_t c = read_uint16(pc);
      printf("make-closure %" PRIu16 "\n", c);
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(literals->rowMajorAref(c));
      size_t nclosed = fn->environmentSize();
      printf("  nclosed = %zu\n", nclosed);
      ClosureWithSlots_sp closure
        = ClosureWithSlots_O::make_bytecode_closure(fn, nclosed);
      // FIXME: Can we use some more abstracted access?
      vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(nclosed);
      vm.push(closure.raw_());
      pc++;
      break;
    }
    case vm_make_uninitialized_closure: {
      uint16_t c = read_uint16(pc);
      printf("make-uninitialized-closure %" PRIu16 "\n", c);
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(literals->rowMajorAref(c));
      size_t nclosed = fn->environmentSize();
      printf("  nclosed = %zu\n", nclosed);
      ClosureWithSlots_sp closure
        = ClosureWithSlots_O::make_bytecode_closure(fn, nclosed);
      vm.push(closure.raw_());
      pc++;
      break;
    }
    case vm_initialize_closure: {
      uint16_t c = read_uint16(pc);
      printf("initialize-closure %" PRIu16 "\n", c);
      T_sp tclosure((gctools::Tagged)(*(vm.reg(c))));
      ClosureWithSlots_sp closure = gc::As<ClosureWithSlots_sp>(tclosure);
      // FIXME: We ought to be able to get the closure size directly
      // from the closure through some nice method.
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(closure->entryPoint());
      size_t nclosed = fn->environmentSize();
      printf("  nclosed = %zu\n", nclosed);
      vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(nclosed);
      pc++;
      break;
    }
    case vm_return: {
      printf("return\n");
      size_t numValues = vm.npushed(nlocals);
      printf("  numValues = %zu\n", numValues);
      if (numValues == 0) { // Return mv register as-is.
        vm.pop_frame(nlocals);
        core::MultipleValues &mv = core::lisp_multipleValues();
        size_t nvalues = mv.getSize();
        return gctools::return_type(mv.valueGet(0, nvalues).raw_(), nvalues);
      } else {
        vm.copyto(numValues - 1, &my_thread->_MultipleValues._Values[1]);
        vm.drop(numValues - 1);
        T_O* primary = vm.pop();
        vm.pop_frame(nlocals);
        return gctools::return_type(primary, numValues);
      }
    }
    case vm_bind_required_args: {
      uint16_t nargs = read_uint16(pc);
      printf("bind-required-args %" PRIu16 "\n", nargs);
      vm.copytoreg(lcc_args, nargs, 0);
      pc++;
      break;
    }
    case vm_bind_optional_args: {
      uint16_t nreq = read_uint16(pc);
      uint16_t nopt = read_uint16(pc);
      printf("bind-optional-args %" PRIu16 " %" PRIu16 "\n", nreq, nopt);
      if (lcc_nargs >= nreq + nopt) { // enough args- easy mode
        printf("  enough args\n");
        vm.copytoreg(lcc_args + nreq, nopt, nreq);
      } else { // put in some unbounds
        printf("  not enough args\n");
        vm.copytoreg(lcc_args + nreq, lcc_nargs - nreq, nreq);
        vm.fillreg(unbound<T_O>().raw_(), nreq + nopt - lcc_nargs, lcc_nargs);
      }
      pc++;
      break;
    }
    // listify-rest-args 15 parse-key-args 16
    case vm_jump_8: {
      int32_t rel = read_label(pc, 1);
      printf("jump %" PRId32 "\n", rel);
      pc += rel;
      break;
    }
    case vm_jump_16: {
      int32_t rel = read_label(pc, 2);
      printf("jump %" PRId32 "\n", rel);
      // jumps are relative to the first byte of the label, not the last.
      pc += rel - 1;
      break;
    }
    case vm_jump_24: {
      int32_t rel = read_label(pc, 3);
      printf("jump %" PRId32 "\n", rel);
      pc += rel - 2;
      break;
    }
    case vm_jump_if_8: {
      int32_t rel = read_label(pc, 1);
      printf("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) pc += rel;
      else pc++;
      break;
    }
    case vm_jump_if_16: {
      int32_t rel = read_label(pc, 2);
      printf("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) pc += rel - 1;
      else pc++;
      break;
    }
    case vm_jump_if_24: {
      int32_t rel = read_label(pc, 3);
      printf("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) pc += rel -2;
      else pc++;
      break;
    }
    case vm_jump_if_supplied: {
      uint16_t slot = read_uint16(pc);
      int32_t rel = read_label(pc, 3);
      printf("jump-if-supplied %" PRIu16 " %" PRId32 "\n", slot, rel);
      T_sp tval((gctools::Tagged)(*(vm.reg(slot))));
      if (tval.unboundp()) pc++;
      else pc += rel - 2;
      break;
    }
    case vm_check_arg_count_LE_: {
      uint16_t max_nargs = read_uint16(pc);
      printf("check-arg-count<= %" PRIu16 "\n", max_nargs);
      if (lcc_nargs > max_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        throwTooManyArgumentsError(tclosure, lcc_nargs, max_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_GE_: {
      uint16_t min_nargs = read_uint16(pc);
      printf("check-arg-count>= %" PRIu16 "\n", min_nargs);
      if (lcc_nargs < min_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        throwTooFewArgumentsError(tclosure, lcc_nargs, min_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_EQ_: {
      uint16_t req_nargs = read_uint16(pc);
      printf("check-arg-count= %" PRIu16 "\n", req_nargs);
      if (lcc_nargs != req_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        wrongNumberOfArguments(tclosure, lcc_nargs, req_nargs);
      }
      pc++;
      break;
    }
    case vm_push_values: {
      // TODO: Direct copy?
      printf("push-values\n");
      MultipleValues &mv = lisp_multipleValues();
      size_t nvalues = mv.getSize();
      printf("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i) vm.push(mv.valueGet(i, nvalues).raw_());
      // We could skip tagging this, but that's error-prone.
      vm.push(make_fixnum(nvalues).raw_());
      pc++;
      break;
    }
    case vm_append_values: {
      printf("append-values\n");
      T_sp texisting_values((gctools::Tagged)vm.pop());
      size_t existing_values = texisting_values.unsafe_fixnum();
      printf("  existing-values = %zu\n", existing_values);
      MultipleValues &mv = lisp_multipleValues();
      size_t nvalues = mv.getSize();
      printf("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i) vm.push(mv.valueGet(i, nvalues).raw_());
      vm.push(make_fixnum(nvalues + existing_values).raw_());
      pc++;
      break;
    }
    case vm_pop_values: {
      printf("pop-values\n");
      MultipleValues &mv = lisp_multipleValues();
      T_sp texisting_values((gctools::Tagged)vm.pop());
      size_t existing_values = texisting_values.unsafe_fixnum();
      printf("  existing-values = %zu\n", existing_values);
      vm.copyto(existing_values, &my_thread->_MultipleValues._Values[0]);
      mv.setSize(existing_values);
      vm.drop(existing_values);
      pc++;
      break;
    }
    case vm_mv_call: {
      printf("mv-call\n");
      T_O* func = vm.pop();
      size_t nargs = lisp_multipleValues().getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      res.saveToMultipleValue0();
      pc++;
      break;
    }
    case vm_mv_call_receive_one: {
      printf("mv-call-receive-one\n");
      T_O* func = vm.pop();
      size_t nargs = lisp_multipleValues().getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.push(res.raw_());
      pc++;
      break;
    }
    case vm_mv_call_receive_fixed: {
      uint16_t nvals = read_uint16(pc);
      printf("mv-call-receive-fixed %" PRIu16 "\n", nvals);
      T_O* func = vm.pop();
      MultipleValues& mv = lisp_multipleValues();
      size_t nargs = mv.getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      if (nvals != 0) {
        vm.push(res.raw_()); // primary
        size_t svalues = mv.getSize();
        for (size_t i = 1; i < svalues; ++i)
          vm.push(mv.valueGet(i, svalues).raw_());
      }
      pc++;
      break;
    }
    case vm_fdefinition: {
      uint16_t c = read_uint16(pc);
      printf("fdefinition %" PRIu16 "\n", c);
      vm.push(cl__fdefinition(literals->rowMajorAref(c)).raw_());
      pc++;
      break;
    }
    case vm_nil:
        printf("nil\n");
        vm.push(nil<T_O>().raw_());
        pc++;
        break;
    case vm_pop:{
      printf("pop\n");
      T_sp obj((gctools::Tagged)vm.pop());
      core::MultipleValues &mv = core::lisp_multipleValues();
      mv.setSize(1);
      mv.valueSet(0, obj);
      pc++;
      break;
    }
    default:
        SIMPLE_ERROR("Unknown opcode %hu", *pc);
    };
  }
}


};


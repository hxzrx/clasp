import os
import importlib


#
# The wrapper module
#
inspector_mod = None
debugger_mod = None

dir = os.path.dirname(os.path.expanduser(__file__))
print( "\n\n\nLoading clasp udb python extension from directory = %s" % dir)

sys.path.insert(0,dir)

def maybeReloadModules():
    global inspector_mod, debugger_mod
    if (inspector_mod == None):
      inspector_mod = importlib.import_module("clasp_inspect")
    else:
      importlib.reload(inspector_mod)
    if (debugger_mod == None):
      debugger_mod = importlib.import_module("backends.udb")
    else:
      importlib.reload(debugger_mod)
    print(" About to load the clasp_layout")
    inspector_mod.load_clasp_layout(debugger_mod)
  
class LispPrint (gdb.Command):
  def __init__ (self):
    super (LispPrint, self).__init__ ("lprint", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_print(debugger_mod,arg)

class LispHead (gdb.Command):
  def __init__ (self):
    super (LispHead, self).__init__ ("lhead", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_head(debugger_mod,arg)
    
class LispInspect (gdb.Command):
  def __init__ (self):
    super (LispInspect, self).__init__ ("linspect", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_inspect(debugger_mod,arg)
    
class LispFrame (gdb.Command):
  def __init__ (self):
    super (LispFrame, self).__init__ ("lframe", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_frame(debugger_mod,arg)
    
class LispDisassemble (gdb.Command):
  def __init__ (self):
    super (LispDisassemble, self).__init__ ("ldis", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_disassemble(debugger_mod,arg)
    
class LispTest (gdb.Command):
  def __init__ (self):
    super (LispTest, self).__init__ ("ltest", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_test(debugger_mod,arg)
    
class LispVm (gdb.Command):
  def __init__ (self):
    super (LispVm, self).__init__ ("lvm", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_vm(debugger_mod,arg)
    
LispInspect()
LispPrint()
LispHead()
LispDisassemble()
LispTest()
LispFrame()
LispVm()

print("lprint <address>   - print lisp object in compact form")
print("linspect <address> - inspect lisp object - all fields")
print("lhead <address>    - dump the clients header")
print("lframe             - Dump the function name and args for a lisp frame trampoline")
print("ldis <bytecode-module-tptr>    - Disassemble a bytecode-module")
print("ltest <address>    - test module reloading")
print("lvm                - Dump current vm status")
print("python-interactive <expr> - (or pi) interactive Python session\n")

/*
    File: float_to_digits.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.
    Copyright (c) 2012, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/num_co.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>

namespace core {

#define PRINT_BASE clasp_make_fixnum(10)
#define EXPT_RADIX(x) clasp_ash(clasp_make_fixnum(1), x)

typedef struct {
  Real_sp r;
  Real_sp s;
  Real_sp mm;
  Real_sp mp;
  bool high_ok;
  bool low_ok;
} float_approx;

Real_sp times2(Real_sp x) {
  return gc::As<Real_sp>(clasp_plus(x, x));
}

static float_approx *setup(Float_sp number, float_approx *approx) {
  Real_mv mv_f = cl__integer_decode_float(number);
  Integer_sp f = gc::As<Integer_sp>(mv_f);
  MultipleValues& mvn = core::lisp_multipleValues();
  Fixnum_sp fne = gc::As<Fixnum_sp>(mvn.valueGet(1,mv_f.number_of_values()));
  Fixnum e = fne.unsafe_fixnum(), min_e;
  bool limit_f = 0;
  switch (clasp_t_of(number)) {
  case number_SingleFloat:
    min_e = FLT_MIN_EXP;
    limit_f = (unbox_single_float(gc::As<SingleFloat_sp>(number)) ==
               ldexpf(FLT_RADIX, FLT_MANT_DIG - 1));
    break;
  case number_DoubleFloat:
    min_e = DBL_MIN_EXP;
    limit_f = (gc::As<DoubleFloat_sp>(number)->get() ==
               ldexp(FLT_RADIX, DBL_MANT_DIG - 1));
    break;
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    min_e = LDBL_MIN_EXP;
    limit_f = (number.as<LongFloat_O>()->get() ==
               ldexpl(FLT_RADIX, LDBL_MANT_DIG - 1));
#endif
  default:
    SIMPLE_ERROR(("Illegal type"));
  }
  approx->low_ok = approx->high_ok = clasp_evenp(f);
  if (e > 0) {
    Fixnum_sp zz(make_fixnum(1));
    Real_sp be = EXPT_RADIX(e);
    if (limit_f) {
      Real_sp be1 = gc::As<Real_sp>(clasp_times(be, clasp_make_fixnum(FLT_RADIX)));
      approx->r = times2(gc::As<Real_sp>(clasp_times(f, be1)));
      approx->s = clasp_make_fixnum(FLT_RADIX * 2);
      approx->mm = be;
      approx->mp = be1;
    } else {
      approx->r = times2(gc::As<Real_sp>(clasp_times(f, be)));
      approx->s = clasp_make_fixnum(2);
      approx->mm = be;
      approx->mp = be;
    }
  } else if (!limit_f || (e == min_e)) {
    approx->r = times2(f);
    approx->s = times2(EXPT_RADIX(-e));
    approx->mp = clasp_make_fixnum(1);
    approx->mm = clasp_make_fixnum(1);
  } else {
    approx->r = times2(clasp_make_fixnum(FLT_RADIX));
    approx->s = times2(EXPT_RADIX(1 - e));
    approx->mp = clasp_make_fixnum(FLT_RADIX);
    approx->mm = clasp_make_fixnum(1);
  }
  return approx;
}

static Fixnum scale(float_approx *approx) {
  Fixnum k = 0;
  Real_sp x = gc::As<Real_sp>(clasp_plus(approx->r, approx->mp));
  int sign;
  do {
    sign = clasp_number_compare(x, approx->s);
    if (approx->high_ok) {
      if (sign < 0)
        break;
    } else {
      if (sign <= 0)
        break;
    }
    approx->s = gc::As<Real_sp>(clasp_times(approx->s, PRINT_BASE));
    k++;
  } while (1);
  do {
    x = gc::As<Real_sp>(clasp_times(x, PRINT_BASE));
    sign = clasp_number_compare(x, approx->s);
    if (approx->high_ok) {
      if (sign >= 0)
        break;
    } else {
      if (sign > 0)
        break;
    }
    k--;
    approx->r = gc::As<Real_sp>(clasp_times(approx->r, PRINT_BASE));
    approx->mm = gc::As<Real_sp>(clasp_times(approx->mm, PRINT_BASE));
    approx->mp = gc::As<Real_sp>(clasp_times(approx->mp, PRINT_BASE));
  } while (1);
  return k;
}

static StrNs_sp generate(StrNs_sp digits, float_approx *approx) {
  Real_sp d, x;
  gctools::Fixnum digit;
  bool tc1, tc2;
  MultipleValues& mvn = core::lisp_multipleValues();
  do {
    Real_mv mv_d = clasp_truncate2(gc::As<Real_sp>(clasp_times(approx->r, PRINT_BASE)), approx->s);
    d = mv_d;
    approx->r = gc::As<Real_sp>(mvn.valueGet(1,mv_d.number_of_values()));
    approx->mp = gc::As<Real_sp>(clasp_times(approx->mp, PRINT_BASE));
    approx->mm = gc::As<Real_sp>(clasp_times(approx->mm, PRINT_BASE));
    tc1 = approx->low_ok ? clasp_lowereq(approx->r, approx->mm) : clasp_lower(approx->r, approx->mm);
    x = gc::As<Real_sp>(clasp_plus(approx->r, approx->mp));
    tc2 = approx->high_ok ? clasp_greatereq(x, approx->s) : clasp_greater(x, approx->s);
    if (tc1 || tc2) {
      break;
    }
    digits->vectorPushExtend(clasp_make_character(clasp_digit_char(gc::As<Fixnum_sp>(d).unsafe_fixnum(),10)));
  } while (1);
  if (tc2 && !tc1) {
    digit = clasp_safe_fixnum(d) + 1;
  } else if (tc1 && !tc2) {
    digit = clasp_safe_fixnum(d);
  } else if (clasp_lower(times2(approx->r), approx->s)) {
    digit = clasp_safe_fixnum(d);
  } else {
    digit = clasp_safe_fixnum(d) + 1;
  }
  digits->vectorPushExtend(clasp_make_character(clasp_digit_char(digit, 10)));
  return digits;
}

static void
change_precision(float_approx *approx, T_sp tposition, T_sp relativep) {
  if (tposition.nilp())
    return;
  gctools::Fixnum pos;
  Fixnum_sp position = gc::As<Fixnum_sp>(tposition);
  pos = position.unsafe_fixnum();
  if (!relativep.nilp()) {
    Real_sp k = clasp_make_fixnum(0);
    Real_sp l = clasp_make_fixnum(1);
    while (clasp_lower(clasp_times(approx->s, l),
                       clasp_plus(approx->r, approx->mp))) {
      k = gc::As<Real_sp>(clasp_one_plus(k));
      l = gc::As<Real_sp>(clasp_times(l, PRINT_BASE));
    }
    position = gc::As<Real_sp>(clasp_minus(k, position));
    {
      Real_sp e1 = gc::As<Real_sp>(cl__expt(PRINT_BASE, position));
      Real_sp e2 = gc::As<Real_sp>(clasp_divide(e1, clasp_make_fixnum(2)));
      Real_sp e3 = gc::As<Real_sp>(cl__expt(PRINT_BASE, k));
      if (clasp_greatereq(clasp_plus(approx->r, clasp_times(approx->s, e1)),
                          clasp_times(approx->s, e2)))
        position = gc::As<Real_sp>(clasp_one_minus(position));
    }
  }
  {
    Real_sp x = gc::As<Real_sp>(clasp_times(approx->s, cl__expt(PRINT_BASE, position)));
    Real_sp e = gc::As<Real_sp>(clasp_divide(x, clasp_make_fixnum(2)));
    Real_sp low = clasp_max2(approx->mm, e);
    Real_sp high = clasp_max2(approx->mp, e);
    if (clasp_lowereq(approx->mm, low)) {
      approx->mm = low;
      approx->low_ok = 1;
    }
    if (clasp_lowereq(approx->mp, high)) {
      approx->mp = high;
      approx->high_ok = 1;
    }
  }
}

CL_LAMBDA(digits number position relativep);
CL_DECLARE();
CL_DOCSTRING(R"dx(float_to_digits)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__float_to_digits(T_sp tdigits, Float_sp number, T_sp position, T_sp relativep) {
  ASSERT(tdigits.nilp()||gc::IsA<Str8Ns_sp>(tdigits));
  gctools::Fixnum k;
  float_approx approx[1];
  setup(number, approx);
  change_precision(approx, position, relativep);
  k = scale(approx);
  StrNs_sp digits;
  if (tdigits.nilp()) {
    digits = gc::As<StrNs_sp>(core__make_vector(cl::_sym_base_char,
                                                10,
                                                true /* adjustable */,
                                                clasp_make_fixnum(0) /* fill pointer */));
  } else {
    digits = gc::As<StrNs_sp>(tdigits);
  }
  generate(digits, approx);
  return Values(clasp_make_fixnum(k), digits);
}

  SYMBOL_EXPORT_SC_(CorePkg, float_to_digits);

};

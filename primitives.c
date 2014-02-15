/* Defines the globals and primitives in use. Most primitives will be
   defined as a C procedure to be called inlined, and a structure
   (referring to the procedure) to be used as a value. Some of the C
   procedures are wrappers around a macro, where the macro has been
   defined for convenience in C code.
 */

#include "scheme.h"

/* Globals */

SCM_DefineImmediateObject(SCM_nil_object,SCM_NULL_TAG);
SCM_DefineImmediateObject(SCM_true_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_false_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_undefined_object,SCM_UNDEFINED_TAG);

SCM_DefineInitializedGlobal(NIL, "NIL", &SCM_nil_object);
SCM_DefineInitializedGlobal(F, "F", &SCM_false_object);
SCM_DefineInitializedGlobal(T, "T", &SCM_true_object);

/* The structs representing predefined procedures used as values. The
   uppercase names are aliases given in Scheme->C-names-mapping */

SCM_PredefineFunctionVariable(EQNP,"=",2,SCM_eqnp);
SCM_PredefineFunctionVariable(EQP,"EQ?",2,SCM_eqp);
SCM_PredefineFunctionVariable(car,"CAR",1,SCM_car);
SCM_PredefineFunctionVariable(cdr,"CDR",1,SCM_cdr);
SCM_PredefineFunctionVariable(cons,"CONS",2,SCM_cons);
SCM_PredefineFunctionVariable(CONSP,"PAIR?",1,SCM_consp);
SCM_PredefineFunctionVariable(NULLP,"NULL?",1,SCM_nullp);
SCM_PredefineFunctionVariable(PLUS,"PLUS",2,SCM_plus);

SCM_PredefineFunctionVariable(list,"LIST",-1,SCM_list);
SCM_PredefineFunctionVariable(apply,"APPLY",-3,SCM_apply);

SCM_PredefineFunctionVariable(print,"PRINT",1,SCM_print);

/* Apply.

Reminder: (apply f [arg1 ...] args) requires args to be a list, and
calls the procedure f with arguments consisting of `arg1 ...` then the
elements of `args`.
*/

SCM SCM_apply(unsigned long count, va_list arguments) {
  SCM args[31]; /* yes, an arbitrary limit on number of arguments
                   allowed */
  SCM f = va_arg(arguments, SCM);
  if (SCM_2tag(f) != SCM_CLOSURE_TAG &&
      SCM_2tag(f) != SCM_SUBR_TAG) {
    return SCM_error(SCM_ERR_CANNOT_APPLY);
  }

  unsigned long i;
  for (i = 0; i < count - 1; i++) {
    args[i] = va_arg(arguments,SCM);
  }
  SCM last = args[--i]; /* going to overwrite */

  while (SCM_PairP(last)) {
    args[i++] = SCM_Car(last);
    last = SCM_Cdr(last);
  }
  if (!SCM_NullP(last)) {
    return SCM_error(SCM_ERR_APPLY_ARG);
  }

  switch (i) {
  case 0: return SCM_invoke0(f);
  case 1: return SCM_invoke1(f,args[0]);
  case 2: return SCM_invoke2(f,args[0],args[1]);
  case 3: return SCM_invoke3(f,args[0],args[1],args[2]);
  case 4: return SCM_invoke(f,4,args[0],args[1],args[2],args[3]);
  case 5: return SCM_invoke(f,5,args[0],args[1],args[2],args[3],args[4]);
  case 6: return SCM_invoke(f,6,args[0],args[1],args[2],args[3],args[4],args[5]);
  default:
    return SCM_error(SCM_ERR_APPLY_SIZE);
  }
}

/* Runtime primitives based on macros (the functions rather than the
 * structs). The book code has a couple of shortcuts for these, which
 * I'll copy. */

#define DefDyadicFunction(name,macro) \
SCM name(SCM x, SCM y) {              \
  return macro(x, y);                 \
}

#define DefDyadicPred(name,macro) \
SCM name(SCM x, SCM y) {          \
  return (SCM_2bool(macro(x,y))); \
}

#define DefMonadicPred(name,macro) \
SCM name(SCM x) {                  \
  return (SCM_2bool(macro(x)));    \
}

DefMonadicPred(SCM_nullp, SCM_NullP);
DefMonadicPred(SCM_consp, SCM_PairP);
DefMonadicPred(SCM_symbolp, SCM_SymbolP);
DefMonadicPred(SCM_stringp, SCM_StringP);

DefDyadicPred(SCM_eqp, SCM_EqP);

DefDyadicFunction(SCM_plus, SCM_Plus)
DefDyadicFunction(SCM_eqnp, SCM_EqnP)

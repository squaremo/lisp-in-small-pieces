/* Defines the globals and primitives in use */

#include "scheme.h"

/* Globals */

SCM_DefineImmediateObject(SCM_nil_object,SCM_NULL_TAG);
SCM_DefineImmediateObject(SCM_true_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_false_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_undefined_object,SCM_UNDEFINED_TAG);

SCM_DefineInitializedGlobal(NIL, "NIL", &SCM_nil_object);
SCM_DefineInitializedGlobal(F, "F", &SCM_false_object);
SCM_DefineInitializedGlobal(T, "T", &SCM_true_object);

/* Predefined procedures */

SCM_PredefineFunctionVariable(car,"CAR",1,SCM_car);
SCM_PredefineFunctionVariable(cdr,"CDR",1,SCM_cdr);
SCM_PredefineFunctionVariable(cons,"CONS",2,SCM_cons);
SCM_PredefineFunctionVariable(eqn,"=",2,SCM_eqnp);
SCM_PredefineFunctionVariable(eq,"EQ?",2,SCM_eqp);

/* Define in the runtime, because it's used there for other
 * purposes */
extern SCM SCM_list(unsigned long count, va_list arguments);
SCM_PredefineFunctionVariable(list,"LIST",-1,SCM_list);

/* Primitives based on macros */
#define DefDyadicFunction(name,macro) \
SCM name(SCM x, SCM y) {              \
  return macro(x, y);                 \
}

DefDyadicFunction(SCM_plus, SCM_Plus)
DefDyadicFunction(SCM_eqnp, SCM_EqnP)
DefDyadicFunction(SCM_eqp, SCM_EqP)


#ifndef SCHEME_H
#define SCHEME_H

#include <stdarg.h>
#include <stdlib.h>

/* Values */

/* A Scheme value (SCM) is a pointer to the union SCM_object, OR a
 * fixnum */
typedef union SCM_object *SCM;

/* Fixnums are puns of the above, and have the lowest bit set.  All
   other values are pointers to SCM_object, and have the lowest bit
   unset. */
#define SCM_FixnumP(x)    ((unsigned long)(x) & (unsigned long)1)
#define SCM_Fixnum2int(x) ((long)(x) >> 1)
#define SCM_Int2fixnum(x) ((SCM)(((x)<<1) | 1))

/* Values that are pointed to ... */
union SCM_object {
  struct SCM_pair {
    SCM cdr;
    SCM car;
  } pair;
  struct SCM_string {
    char Cstring[8];
  } string;
  struct SCM_symbol {
    SCM pname;
  } symbol;
  struct SCM_box {
    SCM content;
  } box;
  struct SCM_subr {
    SCM (*behaviour)();
    long arity;
  } subr;
  /* See SCM_DefineClosure later */
  struct SCM_closure {
    SCM (*behaviour)();
    long arity;
    SCM environment[1];
  } closure;
  struct SCM_escape {
    struct SCM_jmp_buf *stack_address;
  } escape;
};

/* Each value has a tag that says what type of value it is. This is
 * put in the word *before* the pointer. In the book the value pointers
 * are called "wrapped", and word-behind-value pointers are called
 * "unwrapped". I'm not clear on when each is used .. */

/* These discriminate among the types in the union above. NB they
 * don't take a whole word. */
enum SCM_tag {
  SCM_NULL_TAG      = 0xaaa0,
  SCM_PAIR_TAG      = 0xaaa1,
  SCM_BOOLEAN_TAG   = 0xaaa2,
  SCM_UNDEFINED_TAG = 0xaaa3,
  SCM_SYMBOL_TAG    = 0xaaa4,
  SCM_STRING_TAG    = 0xaaa5,
  SCM_SUBR_TAG      = 0xaaa6,
  SCM_CLOSURE_TAG   = 0xaaa7,
  SCM_ESCAPE_TAG    = 0xaaa8
};
/* The union is just to guarantee that the header takes up a word */
union SCM_header {
  enum SCM_tag tag;
  SCM ignored;
};

/* These are what we'll actually use when creating values; it's the
 * same structs as the SCM_object but with the tag at the front. */
union SCM_unwrapped_object {
  /* .. Apart from this one, which is for things that are singletons,
   * e.g., NIL, true, false. */
  struct SCM_unwrapped_immediate_object {
    union SCM_header header;
  } object;

  struct SCM_unwrapped_pair {
    union SCM_header header;
    SCM cdr;
    SCM car;
  } pair;
  struct SCM_unwrapped_string {
    union SCM_header header;
    char Cstring[8];
  } string;
  struct SCM_unwrapped_symbol {
    union SCM_header header;
    SCM pname;
  } symbol;
  struct SCM_unwrapped_box {
    union SCM_header header;
    SCM content;
  } box;
  struct SCM_unwrapped_subr {
    union SCM_header header;
    SCM (*behaviour)();
    long arity;
  } subr;
  struct SCM_unwrapped_closure {
    union SCM_header header;
    SCM (*behaviour)();
    long arity;
    SCM environment[1];
  } closure;
  struct SCM_unwrapped_escape {
    union SCM_header header;
    struct SCM_jmp_buf *stack_address;
  } escape;
};

typedef union SCM_unwrapped_object *SCMref;

#define SCM_Wrap(x)   ((SCM) (((union SCM_header *) x) + 1))
#define SCM_Unwrap(x) ((SCMref) (((union SCM_header *) x) - 1))
#define SCM_2tag(x)   ((SCM_Unwrap((SCM)x))->object.header.tag)

#define SCM_CfunctionAddress(Cfunction) ((SCM (*)(void)) Cfunction)

/* Defining values (quotations) */

#define SCM_DefinePair(pair, car, cdr) \
  static struct SCM_unwrapped_pair = {{SCM_PAIR_TAG}, cdr, car}
#define SCM_DefineSymbol(symbol, pname)      \
  static struct SCM_unwrapped_symbol symbol = \
    {{SCM_SYMBOL_TAG}, pname}
/* To statically allocate a string, we have to define a data structure
 * of the appropraite size first */
#define SCM_DefineString(Cname,string) \
  struct Cname##_struct {              \
    union SCM_header header;           \
    char CString[1 + sizeof(string)];  \
  };                                   \
  static struct Cname##_struct Cname = \
    {{SCM_STRING_TAG},string}

#define SCM_DefineImmediateObject(name,tag)             \
  struct SCM_unwrapped_immediate_object name = {{tag}}

/* These get used in the generated code, and given values in
   primitives.c and scheme.c */
extern struct SCM_unwrapped_immediate_object SCM_true_object;
extern struct SCM_unwrapped_immediate_object SCM_false_object;
extern struct SCM_unwrapped_immediate_object SCM_nil_object;
extern struct SCM_unwrapped_immediate_object SCM_undefined_object;
#define SCM_true  SCM_Wrap(&SCM_true_object)
#define SCM_false SCM_Wrap(&SCM_false_object)
#define SCM_nil   SCM_Wrap(&SCM_nil_object)
#define SCM_undefined SCM_Wrap(&SCM_undefined_object)

/* Runtime procedures, defined in scheme.c */

extern SCM SCM_invoke(SCM fun, unsigned long number, ...);
extern SCM SCM_close(SCM (*Cfunc)(void), long arity, unsigned long size, ...);
extern SCM SCM_signal_error(unsigned long code, unsigned long line, char *file);
extern SCM SCM_allocate_box(SCM v);
/* extern SCM SCM_allocate_continuation (struct SCM_jmp_buf *address); */
extern SCM SCM_list(unsigned long count, va_list arguments);
extern SCM SCM_prin(SCM x); /* not the primitive, though it uses this */
/* extern SCM SCM_apply(unsigned long number, va_list arguments); */


/* Runtime primitives, the functions rather than the structs. The book
 * code has a couple of shortcuts for these, which I'll copy */

#define SCM_DeclareConstant(var) extern SCM var

#define SCM_DeclareSubr0(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname(void)
#define SCM_DeclareSubr1(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname(SCM x)
#define SCM_DeclareSubr2(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname(SCM x, SCM y)
#define SCM_DeclareSubr3(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname(SCM x, SCM y, SCM z)

SCM_DeclareSubr2(EQN,SCM_eqnp);
SCM_DeclareSubr2(EQ,SCM_eqp);
SCM_DeclareSubr1(CAR,SCM_car);
SCM_DeclareSubr1(CDR,SCM_cdr);
SCM_DeclareSubr2(CONS,SCM_cons);
SCM_DeclareSubr2(PLUS,SCM_plus);
SCM_DeclareSubr1(NULLP,SCM_nullp);

SCM_DeclareSubr1(print, SCM_print);
SCM_DeclareConstant(list);

/* Handy macros for dealing with values from C */

#define SCM_2bool(x) ((x) ? SCM_true : SCM_false)

/* Unsafe pair ops */
#define SCM_Car(x) (SCM_Unwrap(x)->pair.car)
#define SCM_Cdr(x) (SCM_Unwrap(x)->pair.cdr)
#define SCM_NullP(x) ((x) == SCM_nil)
#define SCM_PairP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_PAIR_TAG))

/* Predicates */
#define SCM_SymbolP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_SYMBOL_TAG))
#define SCM_StringP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_STRING_TAG))

#define SCM_EqP(x,y) ((x)==(y))

#define SCM_EqnP(x,y) \
  ((SCM_FixnumP(x) && SCM_FixnumP(y))                 \
  ? SCM_2bool(SCM_Fixnum2int(x) == SCM_Fixnum2int(y)) \
   : SCM_error(SCM_ERR_EQNP))

/* Operations as macros */

#define SCM_Plus(x,y) \
  ((SCM_FixnumP(x) && SCM_FixnumP(y)) \
  ? SCM_Int2fixnum(SCM_Fixnum2int(x) + SCM_Fixnum2int(y)) \
  : SCM_error(SCM_ERR_PLUS))

#define SCM_GtP(x,y) \
  ((SCM_FixnumP(x) && SCM_FixnumP(y)) \
  ? SCM_2bool(SCM_Fixnum2int(x) > SCM_Fixnum2int(y)) \
  : SCM_error(SCM_ERR_GTP))

/* ... TODO */

/* Global variables */

#define SCM_CheckedGlobal(Cname) \
  ((Cname != SCM_undefined) \
   ? Cname : SCM_error(SCM_ERR_UNINITIALIZED))

/* Oddly, used to initialize globals with an undefined value. But
 * there you are. */
#define SCM_DefineInitializedGlobal(Cname,string,value) \
  SCM Cname = SCM_Wrap(value)

#define SCM_DefineGlobalVariable(Cname,string) \
  SCM_DefineInitializedGlobal(Cname,string,&SCM_undefined_object)

/* Predefined functions */

#define SCM_PredefineFunctionVariable(subr,string,arity,Cfunction) \
  static struct SCM_unwrapped_subr subr##_object = \
    {{SCM_SUBR_TAG}, Cfunction, arity}; \
  SCM_DefineInitializedGlobal(subr,string,&(subr##_object))

/* Boxen */

#define SCM_Content(e) ((e)->box.content)

/* Closures and runtime procedures */

/* Each closure has its own struct, with the code as a function
 * pointer to a thunk, the arity, and the free variables. Note that
 * the generated code ends up with the same name -- slightly
 * confusing. */

/* NB: these are punned with SCM_closure, which has the same first two
 * fields and a placeholder for the fields. When allocating a closure,
 * the correct size is chosen and the full set of free variable fields
 * written in. */

#define SCM_DefineClosure(struct_name,fields) \
  struct struct_name {      \
    SCM (*behaviour)(void); \
    long arity;             \
    fields }

/* A runtime procedure is given the closure struct (for free vars),
 * the number of args actually passed, and an array of the
 * arguments. (As the book notes, there's several ways to do
 * invocation, and this is not necessarily the fastest) */

#define SCM_DeclareFunction(Cname) \
  SCM Cname (struct Cname *self_, unsigned long size_, \
             va_list arguments_)
/* va_arg gets the next argument from a va_list, and advances the
   pointer */
#define SCM_DeclareVariable(Cname,rank) \
  SCM Cname = va_arg(arguments_,SCM)
/* Happily there is (will be) already a procedure for forming a list
 * from an array of arguments -- it's just the C form of
 *  (list . args) */
#define SCM_DeclareDottedVariable(Cname,rank) \
  SCM Cname = SCM_list(size_ - rank, arguments_)
/* Free variables come from the closure struct fields */
#define SCM_Free(Cname) ((*self_).Cname)


/* Shorthands for invoke */

#define SCM_invoke0(f)       SCM_invoke(f, 0)
#define SCM_invoke1(f,x)     SCM_invoke(f,1,x)
#define SCM_invoke2(f,x,y)   SCM_invoke(f,2,x,y)
#define SCM_invoke3(f,x,y,z) SCM_invoke(f,3,x,y,z)

/* Given a varargs procedure, figure out how many args are *required*
 * to be given */
#define SCM_MinimalArity(i) (-(i)-1)

/* Error reporting */
#define SCM_error(num) SCM_signal_error(num,__LINE__,__FILE__)

/* May as well use the same scheme as the book */

#define SCM_ERR_PLUS           70
#define SCM_ERR_GTP            75
#define SCM_ERR_EQNP           77

#define SCM_ERR_CAR            60
#define SCM_ERR_CDR            61
#define SCM_ERR_SET_CAR        62
#define SCM_ERR_SET_CDR        63

#define SCM_ERR_CANNOT_APPLY   50
#define SCM_ERR_WRONG_ARITY    51
#define SCM_ERR_INTERNAL       52

#define SCM_ERR_CANT_ALLOC     100
#define SCM_ERR_UNINITIALIZED  11

#endif /* SCHEME_H */

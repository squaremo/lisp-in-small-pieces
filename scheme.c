
#include "scheme.h"
#include <stdio.h>

/* Instantiate a closure */

SCM SCM_close (SCM (*Cfunction)(void),
               long arity, unsigned long size, ...) {
  /* sizeof(SCM_unwrapped_closure) gives enough room to put the
   * header, function pointer, arity, and one field. Each closure
   * struct has its own set of free variable fields, so we need to
   * make space for those as well (minus the one already there). See
   * SCM_DefineClosure in scheme.h */
  SCMref result = (SCMref) malloc(sizeof(struct SCM_unwrapped_closure) +
                                  (size - 1)*sizeof(SCM));
  unsigned long i;
  va_list args;
  if (result == (SCMref)NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  result->closure.header.tag = SCM_CLOSURE_TAG;
  result->closure.behaviour = Cfunction;
  result->closure.arity = arity;
  va_start(args,size);
  for (i = 0; i < size; i++) {
    result->closure.environment[i] = va_arg(args,SCM);
  }
  va_end(args);
  return SCM_Wrap(result);
}

/* Boxeses */

/* As the book code notes, it is kind of ridiculous to malloc a
 * word. It would be better to allocate pools of boxes at a time,
 * perhaps. */
SCM SCM_allocate_box(SCM v) {
  SCM cell = (SCM) malloc(sizeof(struct SCM_box));
  if (cell == (SCM)NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  cell->box.content = v;
  return cell;
}

/* Pairs */

SCM SCM_cons (SCM a, SCM d) {
  SCMref cell = (SCMref) malloc(sizeof(struct SCM_unwrapped_pair));
  if (cell == (SCMref)NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  cell->pair.header.tag = SCM_PAIR_TAG;
  cell->pair.cdr = d;
  cell->pair.car = a;
  return SCM_Wrap(cell);
}

SCM SCM_car (SCM cell) {
  if (SCM_PairP(cell)) {
    return SCM_Car(cell);
  }
  else return SCM_error(SCM_ERR_CAR);
}

SCM SCM_cdr (SCM cell) {
  if (SCM_PairP(cell)) {
    return SCM_Cdr(cell);
  }
  else return SCM_error(SCM_ERR_CDR);
}

SCM SCM_set_cdr (SCM cell, SCM value) {
  if (SCM_PairP(cell)) {
    SCM_Unwrap(cell)->pair.cdr = value;
    return cell;
  }
  else return SCM_error(SCM_ERR_SET_CDR);
}

/* Just for fun, I'm going to take a different approach to the book
 * here, and allocate a list in an array in one go. */
SCM SCM_list(unsigned long count, va_list arguments) {
  if (count == 0) return SCM_nil;

  struct SCM_unwrapped_pair *cells =
    malloc(count * sizeof(struct SCM_unwrapped_pair));
  unsigned long i;
  for (i = 0; i < count; i++) {
    struct SCM_unwrapped_pair *cell = &cells[i];
    cell->header.tag = SCM_PAIR_TAG;
    cell->car = va_arg(arguments,SCM);
    if (i + 1 < count) {
      cell->cdr = SCM_Wrap(&cells[i+1]);
    }
    else {
      cell->cdr = SCM_nil;
    }
  }
  return SCM_Wrap(cells);
}

/* For debug, and main output */
SCM SCM_prin(SCM x);

/* Prin a value in list position */
void SCM_prin_list(SCM x) {
  if (SCM_FixnumP(x)) {
    fprintf(stdout, " . %ld", SCM_Fixnum2int(x));
  }
  else {
    switch (SCM_2tag(x)) {
    case SCM_NULL_TAG: {
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout, " ");
      SCM_prin(SCM_Car(x));
      SCM_prin_list(SCM_Cdr(x));
      break;
    }
    default: {
      fprintf(stdout, " . ");
      SCM_prin(x);
      break;
    }
    }
  }
}

SCM SCM_prin(SCM x) {
  if (SCM_FixnumP(x)) {
    fprintf(stdout, "%ld", SCM_Fixnum2int(x));
  }
  else {
    switch (SCM_2tag(x)) {
    case SCM_NULL_TAG: {
      fprintf(stdout, "()");
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout, "(");
      SCM_prin(SCM_Car(x));
      SCM_prin_list(SCM_cdr(x));
      fprintf(stdout, ")");
      break;
    }
    case SCM_BOOLEAN_TAG: {
      fprintf(stdout, "#%c", (SCM_EqP(x,SCM_true) ? 't' : 'f'));
      break;
    }
    case SCM_UNDEFINED_TAG: {
      fprintf(stdout, "#<undefined>");
      break;
    }
    case SCM_SYMBOL_TAG: {
      SCM str = SCM_Unwrap(x)->symbol.pname;
      char *Cstring = SCM_Unwrap(str)->string.Cstring;
      fprintf(stdout, "%s", Cstring);
      break;
    }
    case SCM_STRING_TAG: {
      char *Cstring = SCM_Unwrap(x)->string.Cstring;
      fprintf(stdout,"\"%s\"", Cstring);
      break;
    }
    case SCM_SUBR_TAG: {
      fprintf(stdout, "#<procedure@%p>", (void *)(x));
      break;
    }
    case SCM_CLOSURE_TAG: {
      fprintf(stdout, "#<closure@%p>", (void *)(x));
      break;
    }
    case SCM_ESCAPE_TAG: {
      fprintf(stdout, "#<continuation@%p>", (void *)(x));
      break;
    }
    default:
      fprintf(stdout, "<something@%p>", (void *)(x));
      break;
    }
  }
  return (x);
}

SCM SCM_print(SCM x) {
  SCM_prin(x);
  printf("\n");
  return (x);
}

SCM SCM_signal_error(unsigned long code, unsigned long line, char* file) {
  fprintf(stdout, "Error %ld in %s:%ld", code, file, line);
  exit(code);
}

/* Now for the big one, invoke. */
SCM SCM_invoke(SCM function, unsigned long number, ...) {
  if (SCM_FixnumP(function)) {
    return SCM_error(SCM_ERR_CANNOT_APPLY);
  }
  else {
    switch (SCM_2tag(function)) {
    case SCM_SUBR_TAG: {
      SCM (*behaviour)(void) = (SCM_Unwrap(function)->subr).behaviour;
      long arity = (SCM_Unwrap(function)->subr).arity;
      SCM result;
      if (arity >= 0) { // no varargs
        if (arity != number) {
          return SCM_error(SCM_ERR_WRONG_ARITY);
        }
        else {
          if (arity == 0) {
            result = behaviour();
          }
          else {
            va_list args;
            va_start(args, number);
            /* ugly, but perhaps necessary */
            SCM a0 = va_arg(args,SCM);
            if (number == 1) {
              result = ((SCM (*)(SCM))*behaviour)(a0);
            }
            else {
              SCM a1 = va_arg(args,SCM);
              if (number == 2) {
                result = ((SCM (*)(SCM,SCM))*behaviour)(a0, a1);
              }
              else {
                SCM a2 = va_arg(args,SCM);
                if (number == 3) {
                  result = ((SCM (*)(SCM,SCM,SCM))*behaviour)(a0, a1, a2);
                }
                else {
                  return SCM_error(SCM_ERR_INTERNAL);
                }
              }
            }
            va_end(args);
          }
          return result;
        }
      }
      else { /* varargs */
        long min_arity = SCM_MinimalArity(arity);
        if (number < min_arity) {
          return SCM_error(SCM_ERR_WRONG_ARITY);
        }
        else {
          va_list args;
          SCM result;
          va_start(args, number);
          result = ((SCM (*)(unsigned long, va_list))
                    *behaviour)(number, args);
          va_end(args);
          return result;
        }
      }
    } /* case SCM_SUBR_TAG */
    case SCM_CLOSURE_TAG: {
      SCM (*behaviour)(void) = (SCM_Unwrap(function)->closure).behaviour;
      long arity = (SCM_Unwrap(function)->closure).arity;
      SCM result;
      va_list args;
      va_start(args,number);
      if (arity >= 0) {
        if (arity != number) {
          return SCM_error(SCM_ERR_WRONG_ARITY);
        }
        else {
          result = ((SCM (*)(SCM,unsigned long, va_list)) *behaviour)
            (function, number, args);
        }
      }
      else { /* varargs */
        long min_arity = SCM_MinimalArity(arity);
        if (number < min_arity) {
          return SCM_error(SCM_ERR_WRONG_ARITY);
        }
        else {
          result = ((SCM (*)(SCM,unsigned long, va_list)) *behaviour)
            (function, number, args);
        }
      }
      va_end(args);
      return result;
    } /* case SCM_CLOSURE_TAG */
    default:
      return SCM_error(SCM_ERR_CANNOT_APPLY);
    }
  }
}

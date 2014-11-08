#ifndef CSTUBS_REMOTE_INTERNALS_H
#define CSTUBS_REMOTE_INTERNALS_H

#ifndef CSTUBS_BUFFER
#error "Please #define CSTUBS_BUFFER"
#endif

#ifndef cstubs_acquire_lock
#error "Please #define cstubs_acquire_lock"
#endif

#ifndef cstubs_release_lock
#error "Please #define cstubs_release_lock"
#endif

#define CTYPES_FUNCALL(F, ...)  \
  (*((struct F ## _frame *)CSTUBS_BUFFER) =    \
   (struct F ## _frame){             \
    F ## _name, __VA_ARGS__          \
   },                                \
   cstubs_release_lock(CSTUBS_BUFFER),         \
   ((struct F ## _frame *)CSTUBS_BUFFER)->return_value)

#endif /* CSTUBS_REMOTE_INTERNALS_H */

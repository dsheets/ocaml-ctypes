#ifndef CSTUBS_REMOTE_INTERNALS_H
#define CSTUBS_REMOTE_INTERNALS_H

#ifndef CSTUBS_BUFFER
#error "Please #define CSTUBS_BUFFER"
#endif

#ifndef CSTUBS_RET_LOCK
#error "Please #define CSTUBS_RET_LOCK"
#endif

#ifndef CSTUBS_ARG_LOCK
#error "Please #define CSTUBS_ARG_LOCK"
#endif

#ifndef cstubs_acquire_lock
#error "Please #define cstubs_acquire_lock"
#endif

#ifndef cstubs_release_lock
#error "Please #define cstubs_release_lock"
#endif

#ifndef cstubs_initialize_shared_memory
#error "Please #define cstubs_initialize_shared_memory"
#endif

#ifndef cstubs_initialize_arg_lock
#error "Please #define cstubs_initialize_arg_lock"
#endif

#ifndef cstubs_initialize_ret_lock
#error "Please #define cstubs_initialize_ret_lock"
#endif

#ifndef cstubs_start_remote_process
#error "Please #define cstubs_start_remote_process"
#endif

#define CTYPES_FUNCALL(F, ...)  \
  (*((struct F ## _frame *)CSTUBS_BUFFER) =    \
   (struct F ## _frame){             \
    F ## _name, __VA_ARGS__          \
   },                                \
   cstubs_release_lock(CSTUBS_ARG_LOCK),         \
   cstubs_acquire_lock(CSTUBS_RET_LOCK),         \
   ((struct F ## _frame *)CSTUBS_BUFFER)->return_value)

#endif /* CSTUBS_REMOTE_INTERNALS_H */

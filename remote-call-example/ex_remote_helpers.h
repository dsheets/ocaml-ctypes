#ifndef EX_REMOTE_HELPERS_H
#define EX_REMOTE_HELPERS_H

#define _BSD_SOURCE

#include <semaphore.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>

/* warning: gnuish braced group */
#define cstubs_initialize_shared_memory(mem)                              \
  ((void)({                                                               \
     int fd_ ## __LINE__ = shm_open(EX_SHMEM_NAME, O_CREAT|O_RDWR, 0666); \
     ftruncate(fd_ ## __LINE__, ex_frame_size);                           \
     mem = mmap(NULL, ex_frame_size, PROT_READ|PROT_WRITE,                \
                MAP_SHARED, fd_##__LINE__, 0);                            \
    }))

#define cstubs_start_remote_process()                        \
  ((fork() == 0                                              \
 ? (void)execl("ex_remote_server", "ex_remote_server", NULL) \
 : (void)0))

#define cstubs_initialize_arg_lock(lock) \
   ((void)(lock = sem_open(EX_SEMAPHORE_ARG_NAME, O_CREAT|O_WRONLY, 0666, 0)))
#define cstubs_initialize_ret_lock(lock) \
   ((void)(lock = sem_open(EX_SEMAPHORE_RET_NAME, O_CREAT|O_RDONLY, 0666, 0)))
#define cstubs_acquire_lock(x) \
  (sem_wait(x))
#define cstubs_release_lock(x) \
  (sem_post(x))

extern void *ex_global_buffer, *ex_global_arg_lock, *ex_global_ret_lock;
#define CSTUBS_ARG_LOCK ex_global_arg_lock
#define CSTUBS_RET_LOCK ex_global_ret_lock
#define CSTUBS_BUFFER ex_global_buffer

#endif /* EX_REMOTE_HELPERS_H */

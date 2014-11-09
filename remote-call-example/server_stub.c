#include <semaphore.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>

#include "ex_shared_definitions.h"

void ex_dispatch(int *call_buffer, void *arglock, void *retlock);

int main(int argc, char **argv)
{
  /* Initialize the semaphores */
  sem_t *argsem = sem_open(EX_SEMAPHORE_ARG_NAME, O_RDONLY);
  if (argsem == NULL) {
    perror("sem_open");
    return EXIT_FAILURE;
  }
  sem_t *retsem = sem_open(EX_SEMAPHORE_RET_NAME, O_WRONLY);
  if (retsem == NULL) {
    perror("sem_open");
    return EXIT_FAILURE;
  }
  
  /* initialize the shared memory */ 
  int shm_fd = shm_open(EX_SHMEM_NAME, O_RDWR, 0666);
  void *ex_buffer = mmap(NULL, ex_frame_size, PROT_READ|PROT_WRITE,
                         MAP_SHARED, shm_fd, 0);
  if (ex_buffer == NULL) {
    perror("mmap");
    return EXIT_FAILURE;
  }

  ex_dispatch(ex_buffer, argsem, retsem);
}

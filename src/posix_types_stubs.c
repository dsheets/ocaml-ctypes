#include <caml/mlvalues.h>

#include <assert.h>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include <stdint.h>

enum arithmetic {
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float,
  Double,
};

#define FLOATING_FLAG_BIT 15
#define UNSIGNED_FLAG_BIT 14
#define FLOATING ((size_t)1u << FLOATING_FLAG_BIT)
#define UNSIGNED ((size_t)1u << UNSIGNED_FLAG_BIT)
#define CHECK_FLOATING(TYPENAME) \
  ((unsigned)(((TYPENAME) 0.5) != 0) << FLOATING_FLAG_BIT)
#define CHECK_UNSIGNED(TYPENAME) \
  ((unsigned)(((TYPENAME) -1) > 0) << UNSIGNED_FLAG_BIT)
#define CLASSIFY(TYPENAME) (CHECK_FLOATING(TYPENAME) | CHECK_UNSIGNED(TYPENAME))
#define ARITHMETIC_TYPEINFO(TYPENAME) (CLASSIFY(TYPENAME) | sizeof(TYPENAME))

static enum arithmetic _underlying_type(size_t typeinfo)
{
  switch (typeinfo)
  {
  case FLOATING | sizeof(float):    return Float;
  case FLOATING | sizeof(double):   return Double;
  case UNSIGNED | sizeof(uint8_t):  return Uint8;
  case UNSIGNED | sizeof(uint16_t): return Uint16;
  case UNSIGNED | sizeof(uint32_t): return Uint32;
  case UNSIGNED | sizeof(uint64_t): return Uint64;
  case            sizeof(int8_t):   return Int8;
  case            sizeof(int16_t):  return Int16;
  case            sizeof(int32_t):  return Int32;
  case            sizeof(int64_t):  return Int64;
  default: assert(0);
  }
}

#define EXPOSE_TYPEINFO(TYPENAME)                            \
  value ctypes_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    size_t typeinfo = ARITHMETIC_TYPEINFO(TYPENAME);         \
    enum arithmetic underlying = _underlying_type(typeinfo); \
    return Val_int(underlying);                              \
  }

#define EXPOSE_TYPESIZE(TYPENAME)              \
  value ctypes_sizeof_ ## TYPENAME(value unit) \
  {                                            \
    return Val_int(sizeof(TYPENAME));          \
  }

EXPOSE_TYPEINFO(blkcnt_t)
EXPOSE_TYPEINFO(blksize_t)
EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO(dev_t)
EXPOSE_TYPEINFO(fsblkcnt_t)
EXPOSE_TYPEINFO(fsfilcnt_t)
EXPOSE_TYPEINFO(gid_t)
EXPOSE_TYPEINFO(id_t)
EXPOSE_TYPEINFO(ino_t)
EXPOSE_TYPEINFO(mode_t)
EXPOSE_TYPEINFO(nlink_t)
EXPOSE_TYPEINFO(off_t)
EXPOSE_TYPEINFO(pid_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO(suseconds_t)
EXPOSE_TYPEINFO(time_t)
EXPOSE_TYPEINFO(uid_t)
EXPOSE_TYPEINFO(useconds_t)

EXPOSE_TYPESIZE(key_t)
EXPOSE_TYPESIZE(pthread_t)
EXPOSE_TYPESIZE(pthread_attr_t)
EXPOSE_TYPESIZE(pthread_cond_t)
EXPOSE_TYPESIZE(pthread_condattr_t)
EXPOSE_TYPESIZE(pthread_key_t)
EXPOSE_TYPESIZE(pthread_mutex_t)
EXPOSE_TYPESIZE(pthread_mutexattr_t)
EXPOSE_TYPESIZE(pthread_once_t)
EXPOSE_TYPESIZE(pthread_rwlock_t)
EXPOSE_TYPESIZE(pthread_rwlockattr_t)
EXPOSE_TYPESIZE(sigset_t)
#ifndef PLATFORM_H_INCLUDED

#if defined(ANDROID) || defined(__ANDROID__)
#  define PLATFORM_MOBILE
#else
#  define PLATFORM_PC
#endif

#ifdef PLATFORM_PC
#  define OnPC(...) __VA_ARGS__
#else
#  define OnPC(...)
#endif

#ifdef PLATFORM_MOBILE
#  define OnMobile(...) __VA_ARGS__
#else
#  define OnMobile(...)
#endif

#endif

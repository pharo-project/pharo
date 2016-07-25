An instance of FFICallbackReturnValue specifies a return value to be passed to a callback callee.  It is intended to have overlaid the following struct:
/*
 * Returning values from callbacks is done through a CallBackReturnSpec
 * which contains a type tag and values.  It is designed to be overlaid upon
 * an FFICallbackReturnProxy created at the Smalltalk level to return values.
 */
typedef struct {
    long type;
# define retint32  0 
# define retint64  1
# define retdouble 2
# define retstruct 3
    long _pad; /* so no doubt that valflt64 & valint32 et al are at byte 8 */
    union {
        long valint32;
        struct { long low, high; } valint64;
        double valflt64;
        struct { void *addr; long size; } valstruct;
    } rvs;
} CallBackReturnSpec;

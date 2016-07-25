OSSUnixSystemAccessor provides access to the operating system in which the Pharo VM is currently running.  There is only one instance of this class, holded by 'OSSVMProcess vmProcess'  which depends on it  to provide access to the operating system process which they represent.

This class provides GENERAL funcionallity for managing files, errors, environments variables, waiting calls, etc etc. Ideally, 100% of its functionallity should be implemented via FFI calls. However, we still use some OSProcess primitives which would be a bit complicaated to implement via FFI (mostly because they access macros, or constants, or things related to a the C pre-processor that we do not have at FFI level). 

The functionallity that is NOT GENERAL (like the call to posix_spawn() family of functions), should not be here but in the concrete place such as OSSUnixSubprocess. 

For the parts that are based on FFI calls, we split each call in two sides. The first side is the method that does the FFI call (under a 'XXX - primitives' protocol, for example, #primitiveFileno:). The other side, is wrapper method that calls the primitive internally but also takes care about managing possible errors of it, informing those, etc (for example, #fileno:). Therefore, is very much likely that the "code users" of this class, will be using the latter side (wrappers) of the methods and not the primitive ones.


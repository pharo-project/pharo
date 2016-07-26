Instances of UnsafeAlien represent the addresses of heap-resident non-pointer Smalltalk objects as actual parameters in FFI call-outs.  An UnsafeAlien on (e.g.) a ByteString used as a parameter in an FFI call causes the FFI machinery to pass the address of the first byte in the ByteString.  THIS IS UNSAFE!  It is unsafe because
a) the garbage collector can potentially move the ByteString (or any other object) during the call, because the call may call-back, invoking the garbage collector,
b) if external code retains the address for longer than the duration of the call and dereferences it in a subsequent call the object may have moved in the mean time,
c) the address of the object is passed without any other potentially necessary conversions such as null-termination
d) the hundred other problems this benighted author hasn't thought of.
Hence UnsafeAlien is to be used carefully by clients that know that the usage is safe.
You have been warned ;)

Create instances via
	UnsafeAlien forPointerTo: 'You are on your own!', (ByteString with: (Character value: 0))
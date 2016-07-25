*FFI-Kernel
integerAt: byteOffset size: nBytes signed: aBoolean
	"Primitive. Return an integer of nBytes size from the receiver.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
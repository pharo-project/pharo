*FFI-Kernel
integerAt: byteOffset put: value size: nBytes signed: aBoolean
	"Primitive. Store the given value as integer of nBytes size
	in the receiver. Fail if the value is out of range.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed
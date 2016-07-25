instance creation
allocate: byteSize
	"Primitive. Allocate an object on the external heap."
	<primitive:'primitiveFFIAllocate' module:'SqueakFFIPrims'>
	^self primitiveFailed
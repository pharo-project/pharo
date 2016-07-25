arithmetic
+ offset
	"Create an address that is offset by the given number of bytes.
	More tricky than one would think due to the FFI's handling of ExternalAddress
	as pointer to an object so that 'self unsignedLongAt: ' would dereference."

	| bytes |
	"Convert xaddr -> bytes"
	bytes := self asByteArrayPointer.
	"Update bytes using platform dependent accessors"
	bytes unsignedLongAt: 1 put: (bytes unsignedLongAt: 1) + offset.
	"Convert bytes -> xaddr"
	^bytes asExternalPointer
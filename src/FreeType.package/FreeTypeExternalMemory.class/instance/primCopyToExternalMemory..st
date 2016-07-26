primitives
primCopyToExternalMemory: aByteArray
	"copy aByteArray into newly allocated, external memory, and store the
	address of that memory in the receiver's handle"
	<primitive: 'primitiveCopyToExternalMemory' module: 'FT2Plugin'>
	^self primitiveFailed
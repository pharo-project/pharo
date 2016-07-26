gc
setGCBiasToGrowGCLimit: aNumber
	"Primitive. Indicate that the bias to grow logic should do a GC after aNumber Bytes"
	<primitive: 'primitiveSetGCBiasToGrowGCLimit'>
	^self primitiveFailed
"Example:
	Smalltalk vm setGCBiasToGrowGCLimit: 16*1024*1024.
"
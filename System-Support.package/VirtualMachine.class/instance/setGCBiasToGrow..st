gc
setGCBiasToGrow: aNumber
	"Primitive. Indicate that the GC logic should be bias to grow"
	<primitive: 'primitiveSetGCBiasToGrow'>
	^self primitiveFailed
"Example:
	Smalltalk vm  setGCBiasToGrowGCLimit: 16*1024*1024.
	Smalltalk vm setGCBiasToGrow: 1.
"
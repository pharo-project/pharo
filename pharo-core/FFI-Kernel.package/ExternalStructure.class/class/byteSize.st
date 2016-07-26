field definition
byteSize
	"Return the size in bytes of this structure."
	^self compiledSpec first bitAnd: FFIStructSizeMask
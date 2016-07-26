accessing
byteSize
	"Return the size in bytes of this type"
	^self headerWord bitAnd: FFIStructSizeMask
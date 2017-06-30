instruction stream support
isExtension: byte
	"Answer if the bytecode is that of an extension bytecodee (a prefix for other bytecodes to extend their range)."
	^self subclassResponsibility
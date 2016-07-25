literals
refersToLiteral: aLiteral 
	"Answer true if any literal in this method is literal, even if embedded in array structure or within its pragmas."
	"only iterate to numLiterals - 1, as the last has the classBinding and the last-but-one needs special treatment"
	
	"to deprecate: use hasLiteralThorough:"
	^ self hasLiteralThorough: aLiteral.
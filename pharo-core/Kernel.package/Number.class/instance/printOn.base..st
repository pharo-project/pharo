printing
printOn: aStream base: base
	"This method should print a representation of the number for the given base,
	excluding the base prefix (and the letter r for radix)"
	
	^self subclassResponsibility
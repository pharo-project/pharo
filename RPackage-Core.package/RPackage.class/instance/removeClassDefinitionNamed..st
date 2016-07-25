private-register
removeClassDefinitionNamed: aClassNameSymbol
	"aClassNameSymbol should be the name of a class and not a metaclass to be coherent with the class invariant that classes are only class names and not metaclass ones. 
	
	This method removes the class definition from the receiver and its organizer"

	| removed |
	removed := classes remove: aClassNameSymbol ifAbsent: [nil].
	removed ifNotNil: [ self unregisterClassName: aClassNameSymbol.] 
	
instance creation
realClass: aClass selector: aString
	"Creates a ring method definition from a Smalltalk class and a selector <compiledMethod>"

	^(aClass>>aString asSymbol) asActiveRingDefinition
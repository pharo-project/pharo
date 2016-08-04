test - accessing source
testIsAbstractExistingMethod
	"testing existing ring method isAbstract"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self deny: method isAbstract.
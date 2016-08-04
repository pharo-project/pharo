test - accessing source
testArgumentNamesExistingMethod
	"accessing ring methods arguments from active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assertCollection: method argumentNames  hasSameElements: #(#aRGBehaviorDefinition #aString) 
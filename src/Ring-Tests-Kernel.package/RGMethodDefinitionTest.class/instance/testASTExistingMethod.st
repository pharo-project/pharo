test - accessing source
testASTExistingMethod
	"accessing ring methods ast from active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assert: method ast equals: method compiledMethod ast.
test - accessing source
testMessagesExistingMethod
	"accessing ring sent messages for active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assert: method messages equals: method compiledMethod messages.
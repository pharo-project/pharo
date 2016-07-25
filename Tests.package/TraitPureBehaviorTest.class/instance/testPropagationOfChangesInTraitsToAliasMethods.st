testing-applying trait composition
testPropagationOfChangesInTraitsToAliasMethods
	| anObject |
	anObject := (self 
				createClassNamed: #AliasTestClass
				superclass: Object
				uses: self t6) new.
	self assert: anObject m22Alias = 22.

	"test update alias method"
	self t2 compile: 'm22 ^17'.
	self assert: anObject m22Alias = 17.

	"removing original method should also remove alias method"
	self t2 removeSelector: #m22.
	self should: [anObject m22Alias] raise: MessageNotUnderstood
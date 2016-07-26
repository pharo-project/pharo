testing-applying trait composition
testPropagationOfChangesInTraitsToAliasMethodsWhenOriginalMethodIsExcluded
	"Assert that alias method is updated although
	the original method is excluded from this user."

	| anObject |
	anObject := (self 
				createClassNamed: #AliasTestClass
				superclass: Object
				uses: self t1 @ { (#aliasM11 -> #m11) } - { #m11 }) new.
	self assert: anObject aliasM11 = 11.
	self deny: (anObject class methodDict includesKey: #m11).
	self t1 compile: 'm11 ^17'.
	self assert: anObject aliasM11 = 17
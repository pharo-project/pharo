tests
testIsSelfEvaluating
	| anotherAssociation |
	self assert: (a isSelfEvaluating).
	
	self assert: (a printString = '1->''one''').
	
	anotherAssociation := Object new -> Object new.
	anotherAssociation isSelfEvaluating ifTrue: [self
		assert: (self class compiler evaluate: anotherAssociation printString) = anotherAssociation
		description: 'a self evaluating should evaluate as self'].

refactoring
selectedMethod
	
	model selectedClass ifNil: [ ^ nil ].
	model selectedMethod ifNil: [ ^ nil ].
	^ model selectedClass >> model selectedMethod
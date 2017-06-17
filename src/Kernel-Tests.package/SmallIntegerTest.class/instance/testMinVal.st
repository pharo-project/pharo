tests - Class Methods
testMinVal
	Smalltalk vm wordSize = 4
		ifTrue: [ self assert: SmallInteger minVal = -16r40000000 ].
	Smalltalk vm wordSize = 8
		ifTrue: [ self assert: SmallInteger minVal = -16r1000000000000000 ]
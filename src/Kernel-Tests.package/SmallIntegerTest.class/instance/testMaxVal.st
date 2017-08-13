tests - Class Methods
testMaxVal
	Smalltalk vm wordSize = 4
		ifTrue: [ self assert: SmallInteger maxVal = 16r3FFFFFFF ].
	Smalltalk vm wordSize = 8
		ifTrue: [ self assert: SmallInteger maxVal = 16rFFFFFFFFFFFFFFF ]
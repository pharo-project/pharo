tests
testIfTrueValue
	| myBooleanObject |
	
	self assert: (MyBooleanObject new ifTrue: [ 1 + 2 ]) equals: '3 sent from my boolean object'.
	myBooleanObject := MyBooleanObject new.
	self assert: (myBooleanObject ifTrue: [ 1 + 2 ]) equals: '3 sent from my boolean object'.
tests
testIfTrueEffect
	| temp fakeBool |
	
	fakeBool := MyBooleanObject new.
	temp := 1.
	fakeBool ifTrue: [ temp := 5 + 3 + 1 ].
	self assert: temp equals: 9.
	
	fakeBool ifTrue: [ [  ] ].
	"fakeBool ifTrue: [ 1+ 2. [ :a | a ] value: 5. 7 ]."
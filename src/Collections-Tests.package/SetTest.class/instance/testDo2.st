tests - iterating
testDo2
	| newFull result |
	newFull := self classToBeTested withAll: (1 to: 5).
	result := 0.
	newFull do: [ :each | result := result + each ].
	self assert: result equals: 15
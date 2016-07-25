tests
testIfNotNil1ArgWithStatement
	self assert: (5 ifNotNil: [ :a | 3. a ]) = 5.
	self assert: (5 ifNotNil: [ :a | a. 3 ]) = 3.
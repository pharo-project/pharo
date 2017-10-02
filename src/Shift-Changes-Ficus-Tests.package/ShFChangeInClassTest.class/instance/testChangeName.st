tests
testChangeName
	| old new changeNameDifference |
	old := self shFTestClassA.
	new := self shFTestClassB.
	results := old compareTo: new.
	changeNameDifference := results
		detect: [ :e | e class = ShFClassRename ]
		ifNone: [ self fail: 'It should have detected the change in the name' ].
	self assert: changeNameDifference oldValue equals: #ShFTestClassA.
	self assert: changeNameDifference newValue equals: #ShFTestClassB
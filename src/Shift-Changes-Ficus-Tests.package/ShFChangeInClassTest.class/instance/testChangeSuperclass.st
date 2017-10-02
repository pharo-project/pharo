tests
testChangeSuperclass
	| old new change |
	old := self shFTestClassA.
	new := self shFTestClassC.
	results := old compareTo: new.
	change := results
		detect: [ :e | e class = ShFSuperclassChange ]
		ifNone: [ self fail: 'It should have detected the change' ].

	self assert: change oldValue equals: #Object.
	self assert: change newValue equals: #ShFTestClassB.
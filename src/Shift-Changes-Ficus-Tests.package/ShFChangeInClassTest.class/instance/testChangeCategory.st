tests
testChangeCategory
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassB.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFCategoryChange ]
		ifNone: [ self fail: 'It should have detected the change in the category' ].

	self assert: categoryChange oldValue equals: #'Shift-Changes-Ficus-Tests-TestClasses'.
	self assert: categoryChange newValue equals: #'Shift-Changes-Ficus-Tests-TestClassesB'
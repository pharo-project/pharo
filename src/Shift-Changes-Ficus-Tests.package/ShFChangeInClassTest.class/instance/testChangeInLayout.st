tests
testChangeInLayout
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassWithByteLayout.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFLayoutTypeChange ]
		ifNone: [ self fail: 'It should have detected the change in the layout' ].

	self assert: categoryChange oldValue equals: #FixedLayout.
	self assert: categoryChange newValue equals: #ByteLayout
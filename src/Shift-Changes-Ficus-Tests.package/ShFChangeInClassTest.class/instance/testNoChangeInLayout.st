tests
testNoChangeInLayout
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassANew.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFLayoutTypeChange ]
		ifFound: [ self fail: 'It should have not detected the change in the layout' ].

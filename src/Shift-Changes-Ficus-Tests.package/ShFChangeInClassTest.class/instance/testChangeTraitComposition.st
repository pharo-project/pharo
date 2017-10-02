tests
testChangeTraitComposition
	| old new categoryChange |
	old := self shFTestClassC.
	new := self shFTestClassCWithoutTrait.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFTraitCompositionChange ]
		ifNone: [ self fail: 'It should have detected the change in the traitComposition ' ].

	self assert: categoryChange oldValue equals: 'TShFTestTrait'.
	self assert: categoryChange newValue equals: '{}'
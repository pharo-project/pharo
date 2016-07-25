examples
exampleFilterableList
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource:
			(FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilter;
		yourself.
	list openInWindow
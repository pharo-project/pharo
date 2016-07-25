function examples
exampleListWithExplicitFilter
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilter;
		explicitFunction;
		yourself.
	list openInWindow
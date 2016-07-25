function examples
exampleListWithExplicitFilterAndActionButton2
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilterWithAction: [ :dataSource :searchPattern :selectedRow | selectedRow browse ];
		yourself.
	list openInWindow
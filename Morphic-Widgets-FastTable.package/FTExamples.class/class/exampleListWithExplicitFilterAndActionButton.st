function examples
exampleListWithExplicitFilterAndActionButton
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilterWithAction: [ :dataSource :searchPattern | Transcript crShow: searchPattern ] named: 'Show';
		yourself.
	list openInWindow
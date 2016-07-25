examples
exampleOutline2
	<example>
	| data list |
	data := OrderedCollection
		withAll:
			(Morph allSubclasses
				collect: [ :each | each allSubclasses sorted: [ :a :b | a name < b name ] ]).
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTOutlineDataSource items: data).
	list openInWindow
tree examples
exampleexampleOneSubLevelTree
	<example>
	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses reject: [ :e | e name endsWith: 'class' ])
				sort: [ :a :b | a name < b name ])
		children: [ :item | item subclasses sort: [ :a :b | a name < b name ] ].
	ds maxDepth: 1.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
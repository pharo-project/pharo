tree examples
exampleTree1
	<example>
	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses reject: [ :e | e name endsWith: 'class' ])
				sort: [ :a :b | a name < b name ])
		children: [ :data | data subclasses sort: [ :a :b | a name < b name ] ].
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
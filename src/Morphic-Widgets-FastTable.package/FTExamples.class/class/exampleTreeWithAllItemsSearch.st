tree examples
exampleTreeWithAllItemsSearch
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 10)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #allItems.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
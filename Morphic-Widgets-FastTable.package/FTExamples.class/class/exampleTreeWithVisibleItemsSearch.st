tree examples
exampleTreeWithVisibleItemsSearch
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #default.	"Optional"
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
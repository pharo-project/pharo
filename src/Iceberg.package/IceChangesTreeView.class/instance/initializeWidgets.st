initialization
initializeWidgets
	tree := self newTree 
		"beCheckList; "
		"autoMultiSelection: true;"
		childrenBlock: [ :changeSet | changeSet elements sorted: [:a :b | a name < b name]];
		displayBlock: [ :changeSet | changeSet description ];
		rootNodeHolder: [ :item |
			IceChangesTreeNodeModel new 
				content: item;
				yourself ];
 		yourself.
		
	self focusOrder add: tree.
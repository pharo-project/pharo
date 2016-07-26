tests
testMultipleSelectionWithChange
	| browser firstTreeMorph secondTreeMorph treeNodeMorph lastTreeNodeMorph |
	browser := GLMWrapper new.
	browser show: [ :a | 
					(a list)
						beMultiple;
						display: [ :x | x - 2 to: x ] ].
	window := browser openOn: 4.
	firstTreeMorph := window submorphs last submorphs first submorphs first submorphs first.
	treeNodeMorph := firstTreeMorph scroller submorphs first.
	lastTreeNodeMorph := firstTreeMorph scroller submorphs last.
	self send: firstTreeMorph mouseUpOnItem: treeNodeMorph.
	self send: firstTreeMorph mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self
		assert: (firstTreeMorph model selection selectedNodePathList collect: [ :each | each first item ]) asOrderedCollection
		equals: #(2 3 4) asOrderedCollection.
	(browser pane port: #entity) value: 42.
	secondTreeMorph := window submorphs last submorphs first submorphs first submorphs first.
	treeNodeMorph := secondTreeMorph scroller submorphs first.
	lastTreeNodeMorph := secondTreeMorph scroller submorphs second.
	self send: secondTreeMorph mouseUpOnItem: treeNodeMorph.
	self send: secondTreeMorph mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self
		assert: (secondTreeMorph model selection selectedNodePathList collect: [ :each | each first item ]) asOrderedCollection
		equals: #(40 41) asOrderedCollection
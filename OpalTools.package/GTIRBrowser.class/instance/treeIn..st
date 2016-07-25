building
treeIn: a
	^ a tree
		rootsExpanded;
		display: [ :anObject | {anObject} ];
		children: [ :each | each children ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | each printString].
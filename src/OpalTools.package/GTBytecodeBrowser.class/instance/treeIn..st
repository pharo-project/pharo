building
treeIn: a
	^ a tree
		rootsExpanded;
		display: [ :anObject | anObject symbolicBytecodes ];
		children: [ :each | each isArray ifTrue: [each] ifFalse: #() ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | each printString].
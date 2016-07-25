initialization
defaultNodeFor: item

	^ TreeNodeModel new
		content: item;
		children: [ self childrenFor: item ];
		hasChildren: [ self hasChildrenFor: item ];
		yourself
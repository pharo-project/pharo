initialization
defaultNodeFor: item

	^ TreeNodePresenter new
		content: item;
		children: [ self childrenFor: item ];
		hasChildren: [ self hasChildrenFor: item ];
		yourself
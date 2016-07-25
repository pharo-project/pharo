instance creation
root: aRootItem children: aBlock
	^ self new
		rootItem: aRootItem;
		childrenBlock: aBlock;
		addSourceToRootItem;
		yourself
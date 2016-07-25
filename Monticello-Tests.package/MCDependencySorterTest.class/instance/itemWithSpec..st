building
itemWithSpec: anArray
	^ MCMockDependentItem new
		name: anArray first;
		provides: anArray second;
		requires: anArray third
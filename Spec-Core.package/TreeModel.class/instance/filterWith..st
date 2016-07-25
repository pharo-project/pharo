api-filtering
filterWith: aFilter
	
	self selection: nil.
	self updateTree.

	self expandAll.
	self changed: #filterWith: with: {aFilter}.
	
	self expandAll.
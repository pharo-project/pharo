items creation
buildPluginsList
	
	tree := (self treeClass model: self) buildPluginsTree.
	tree
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	
	
	^ tree buildContents
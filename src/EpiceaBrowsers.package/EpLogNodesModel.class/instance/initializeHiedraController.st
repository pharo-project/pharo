initialization
initializeHiedraController

	hiedraRulerController := HiRulerController new.
	
	hiedraRulerController treeModel: logNodesTreeModel.
	logNodesTreeModel whenRootsChanged: [ 
		hiedraRulerController updateFromTree ].
	
	hiedraRulerController rulerBuilder
		rowSize: self rowSize;
		columnSize: self rowSize // 3.
	
	hiedraRulerController rulerBuilder addLinkBuilder: [ :linkBuilder |
		linkBuilder targetsBlock: [ :node | node referencedNodes ] ].
	
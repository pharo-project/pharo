private
findFileListIndex: aMorphTreeSelectionChanged 
	| node selectedPath |
	
	selectedPath := aMorphTreeSelectionChanged selection selectedNodePath.
	selectedPath ifNil: [^ self].
	node := selectedPath first.
	self fileListIndex: (self fileList indexOf: node item).

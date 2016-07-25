visiting
visitAssignmentNode: aNode
	self visitVarWrite: aNode variable.
	super visitAssignmentNode: aNode
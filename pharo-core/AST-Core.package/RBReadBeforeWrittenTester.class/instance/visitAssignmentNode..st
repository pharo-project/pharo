visiting
visitAssignmentNode: anAssignmentNode 
	self visitNode: anAssignmentNode value.
	self variableWritten: anAssignmentNode
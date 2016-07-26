visiting
visitAssignmentNode: anAssignmentNode

	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value
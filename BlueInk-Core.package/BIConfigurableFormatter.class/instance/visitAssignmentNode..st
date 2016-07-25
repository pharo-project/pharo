visiting
visitAssignmentNode: anAssignmentNode 
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self visitNode: anAssignmentNode value
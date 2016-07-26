visitor-double dispatching
visitAssignmentNode: anAssignmentNode 
	anAssignmentNode variable: (self visitNode: anAssignmentNode variable).
	anAssignmentNode value: (self visitNode: anAssignmentNode value)
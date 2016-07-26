visitor-double dispatching
visitAssignmentNode: anAssignmentNode 

	super visitAssignmentNode: anAssignmentNode .
	methodBuilder popTop. 
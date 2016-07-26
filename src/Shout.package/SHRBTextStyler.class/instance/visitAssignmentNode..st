visiting
visitAssignmentNode: anAssignmentNode
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value.
	anAssignmentNode variable binding isWritable
		ifFalse: [ self addStyle: #invalid forNode: anAssignmentNode ]
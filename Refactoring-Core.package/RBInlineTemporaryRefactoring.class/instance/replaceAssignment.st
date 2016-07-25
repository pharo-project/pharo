transforming
replaceAssignment
	assignmentNode parent isSequence 
		ifTrue: [assignmentNode parent removeNode: assignmentNode]
		ifFalse: [assignmentNode replaceWith: assignmentNode value]
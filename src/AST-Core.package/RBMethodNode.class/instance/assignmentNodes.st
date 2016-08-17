accessing
assignmentNodes
	^self allChildren select: [:each | each isAssignment].
transforming
addTemporary: sourceNode assignedTo: replacementNode 
	| newName |
	newName := self renameConflictingTemporary: sourceNode name.
	(inlineParseTree body)
		addTemporaryNamed: newName;
		addNodeFirst: (RBAssignmentNode variable: (RBVariableNode named: newName)
					value: replacementNode)
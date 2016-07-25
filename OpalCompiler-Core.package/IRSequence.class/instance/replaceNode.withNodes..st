replacing
replaceNode: aNode withNodes: aCollection 

	self addInstructions: aCollection before: aNode.
	sequence remove: aNode ifAbsent: [self error].
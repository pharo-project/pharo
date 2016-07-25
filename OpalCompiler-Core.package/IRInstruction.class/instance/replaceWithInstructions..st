replacing
replaceWithInstructions: aCollection 

	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNodes: aCollection
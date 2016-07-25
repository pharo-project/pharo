replacing
replaceWith: aNode
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNode: aNode
replacing
replaceWith: aNode 
	parent ifNil: [self error: 'This node doesn''t have a parent'].
	self replaceMethodSource: aNode.
	parent replaceNode: self withNode: aNode
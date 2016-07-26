visiting
visitSequenceNode: aSequenceNode
	self formatTemporariesFor: aSequenceNode.
	self formatCommentsFor: aSequenceNode.
	self formatSequenceNodeStatementsFor: aSequenceNode
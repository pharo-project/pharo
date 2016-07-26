accessing
asReturn
	"Change the current node to a return node."

	parent ifNil: [self error: 'Cannot change to a return without a parent node.'].
	parent isSequence 
		ifFalse: [self error: 'Parent node must be a sequence node.'].
	(parent isLast: self) ifFalse: [self error: 'Return node must be last.'].
	^parent addReturn
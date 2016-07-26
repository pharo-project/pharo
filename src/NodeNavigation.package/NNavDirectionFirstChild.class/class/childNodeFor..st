private
childNodeFor: node
	"We find the child for a node, if it has not childs nil, if has more than one return the first one."
	node ifNil:[^nil].
	^ node children isEmpty
		ifTrue: [ nil ]
		ifFalse: [ node children at: 1]

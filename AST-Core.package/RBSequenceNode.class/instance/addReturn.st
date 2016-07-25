accessing
addReturn
	| node |
	statements isEmpty
		ifTrue: [ ^ nil ].
	statements last isReturn
		ifTrue: [ ^ statements last ].
	node := RBReturnNode value: statements last.
	statements at: statements size put: node.
	node parent: self.
	^ node
running
run: aNode

	| fifo visited ancestors |
	visited := Set new.
	ancestors := aNode previousNodes.
	aNode bfsAncestor: nil.
	fifo := OrderedCollection with: aNode.
	[ancestors isEmpty] whileFalse: [ |p|
		p := fifo removeFirst.
		p nextNodes do: [:next|
			((visited includes: next) or: [fifo includes: next])
				ifFalse: [
					next bfsAncestor: p.
					fifo addLast: next ].
			(ancestors includes: next)
				ifTrue: [ |c|
					c := self retrieveCircuit: next.
					self addCircuit: (self normalize: c).
					ancestors remove: next ]
		].
		visited add: p ]
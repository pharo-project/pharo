actions
startBfsFrom: stronglyConnectedComponents For: parents
	| dequeue childrens |
	dequeue := self dequeue.
	childrens := self childrensFrom: stronglyConnectedComponents for: dequeue.
	childrens
		do: [ :children | 
			(self isVisitedOrInQueue: children)
				ifFalse: [ 
					children bfsParent: dequeue.
					self enqueue: children ].
			(parents includes: children)
				ifTrue: [ 
					self buildCycleUntil: children.
					parents remove: children ] ].
	self addVisitedNodes: dequeue
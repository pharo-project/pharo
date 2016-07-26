querying
bestNodeFor: anInterval 
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	assignment ifNil: [^super bestNodeFor: anInterval].
	((anInterval first between: assignment and: assignment + 1) 
		or: [assignment between: anInterval first and: anInterval last]) 
			ifTrue: [^self].
	self children do: 
			[:each | 
			| node |
			node := each bestNodeFor: anInterval.
			node notNil ifTrue: [^node]]
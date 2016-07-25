querying
bestNodeFor: anInterval 
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	self keywordsIntervals do:
			[:each | 
			((each rangeIncludes: anInterval first) 
				or: [anInterval rangeIncludes:  each first]) 
					ifTrue: [^self]].
	self children do: 
			[:each | 
			| node |
			node := each bestNodeFor: anInterval.
			node notNil ifTrue: [^node]]
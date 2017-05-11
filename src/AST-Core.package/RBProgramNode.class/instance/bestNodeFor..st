querying
bestNodeFor: anInterval 

	| selectedChildren |
	
	anInterval ifNil: [ ^ nil ].
	
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	
	selectedChildren := self children select: [:each | 
		each intersectsInterval: anInterval
	].

	^ selectedChildren size = 1 
		ifTrue: [ selectedChildren first bestNodeFor: anInterval ]
		ifFalse: [ self ]
wiw support
shouldGetStepsFrom: aWorld
	(self isWorldMorph and: [owner notNil]) 
		ifTrue: [ ^self outermostWorldMorph == aWorld ].
	^super shouldGetStepsFrom: aWorld
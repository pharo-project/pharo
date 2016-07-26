accessing
linePosition
	"Answer the position of receiver, relative to line start. "

	(self span isLineSeparator and: [self spanPosition =1 ]) ifTrue: [ ^ 0 ].
	
	self span lineStartSpan spanAndPositionDo: [ :newSpan :spanStartPosition |
		newSpan = self span 
			ifTrue: [ ^ spanStartPosition + self spanPosition ]].
		
	^ self invalidPosition 

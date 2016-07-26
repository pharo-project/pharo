api
emphasis: anEmphasis
	anEmphasis isInteger
		ifTrue:
			[ 
			self deprecated: (self emphasisDepractedFor: anEmphasis).
			^ self emphasis: (self emphasisForCode: anEmphasis) ].
	emphasis := anEmphasis isSymbol
		ifTrue: [ {anEmphasis} ]
		ifFalse: [ anEmphasis ].
	self changed: #emphasis: with: {emphasis}
private
methodOrBlockTempDeclStyleFor: aSequenceNode
	^ aSequenceNode scope isBlockScope
		ifTrue: [ #blockPatternTempVar ]
		ifFalse: [ #patternTempVar ]
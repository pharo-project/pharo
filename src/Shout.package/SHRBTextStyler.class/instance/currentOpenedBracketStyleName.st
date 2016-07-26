private
currentOpenedBracketStyleName
	bracketLevel isZero ifTrue: [ ^ #blockStart ].
	^ (#blockStart asString , bracketLevel asString) asSymbol
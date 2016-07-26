private
currentClosedBracketStyleName
	bracketLevel isZero ifTrue: [ ^ #blockEnd ].
	^ (#blockEnd asString , bracketLevel asString) asSymbol
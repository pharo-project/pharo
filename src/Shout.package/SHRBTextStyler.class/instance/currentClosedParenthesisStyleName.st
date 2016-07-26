private
currentClosedParenthesisStyleName
	parentheseLevel isZero ifTrue: [ ^ #rightParenthesis ].
	^ (#rightParenthesis asString , parentheseLevel asString) asSymbol
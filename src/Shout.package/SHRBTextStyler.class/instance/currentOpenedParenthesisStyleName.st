private
currentOpenedParenthesisStyleName
	parentheseLevel isZero ifTrue: [ ^ #leftParenthesis ].
	^ (#leftParenthesis asString , parentheseLevel asString) asSymbol
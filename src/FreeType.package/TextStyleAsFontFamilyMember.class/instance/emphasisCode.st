squeak emphasis
emphasisCode
	" Answer the squeak emphasis code (1=bold, 2=italic, 3=boldItalic etc"
	| answer |
	answer := 0.
	(styleName includesSubstring: 'Italic') 
		ifTrue:[answer := answer bitOr: LogicalFont squeakSlantItalic].
	(styleName includesSubstring: 'Condensed')
		ifTrue:[answer := answer bitOr: LogicalFont squeakStretchCondensed].
	(styleName includesSubstring: 'Bold')
		ifTrue:[answer := answer bitOr: LogicalFont squeakWeightBold].
	^answer
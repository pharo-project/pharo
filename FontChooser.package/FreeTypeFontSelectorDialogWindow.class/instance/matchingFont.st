as yet unclassified
matchingFont
	"Answer the font that matches the selections."

	|lf emp|
	self familyName ifNil: [^TextStyle defaultFont].
	lf := LogicalFont
		familyName: self familyName 
		pointSize: (self fontSize ifNil: [10]).
	emp := self isBold
		ifTrue: [TextEmphasis bold emphasisCode]
		ifFalse: [TextEmphasis normal emphasisCode].
	self isItalic
		ifTrue: [emp := emp + TextEmphasis italic emphasisCode].
	self isUnderlined
		ifTrue: [emp := emp + TextEmphasis underlined emphasisCode].
	self isStruckOut
		ifTrue: [emp := emp + TextEmphasis struckOut emphasisCode].
	lf := lf emphasis: emp.
	lf realFont ifNil: [^TextStyle defaultFont].
	^lf 
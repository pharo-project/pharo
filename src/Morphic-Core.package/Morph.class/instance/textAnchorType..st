text-anchor
textAnchorType: aSymbol
	aSymbol == #document
		ifTrue:[^self removeProperty: #textAnchorType]
		ifFalse:[^self setProperty: #textAnchorType toValue: aSymbol].
LogicalFont emphasis
stretchValue
	^(styleName includesSubstring: 'Condensed') 
		ifTrue:[LogicalFont stretchCompressed]
		ifFalse:[LogicalFont stretchRegular]
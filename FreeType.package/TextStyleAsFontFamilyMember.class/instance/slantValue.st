LogicalFont emphasis
slantValue
	^(styleName includesSubstring: 'Italic') 
		ifTrue:[LogicalFont slantItalic] 
		ifFalse:[LogicalFont slantRegular]
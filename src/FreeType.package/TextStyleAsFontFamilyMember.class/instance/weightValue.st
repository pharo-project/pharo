LogicalFont emphasis
weightValue
	^(styleName includesSubstring: 'Bold')
		ifTrue:[LogicalFont weightBold] 
		ifFalse:[LogicalFont weightRegular]
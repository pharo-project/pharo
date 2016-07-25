converting
asLogicalFontOfPointSize: pointSize
	^LogicalFont
		familyName: self family familyName 
		pointSize: pointSize 
		stretchValue: self stretchValue
		weightValue: self weightValue
		slantValue: self slantValue
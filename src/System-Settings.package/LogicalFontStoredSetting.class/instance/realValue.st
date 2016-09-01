accessing
realValue
	^ (self class environment at: self fontClassName)
				familyName: self familyName 
				fallbackFamilyNames: self fallbackFamilyNames 
				pointSize: self pointSize 
				stretchValue: self stretchValue
				weightValue: self weightValue 
				slantValue: self slantValue
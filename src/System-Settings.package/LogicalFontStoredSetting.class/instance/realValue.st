accessing
realValue
	^ self fontClassName asClass
				familyName: self familyName 
				fallbackFamilyNames: self fallbackFamilyNames 
				pointSize: self pointSize 
				stretchValue: self stretchValue
				weightValue: self weightValue 
				slantValue: self slantValue
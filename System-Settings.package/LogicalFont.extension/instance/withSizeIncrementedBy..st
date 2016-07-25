*system-settings-browser
withSizeIncrementedBy: anInteger
	^ self class 
		familyName: self familyName 
		fallbackFamilyNames: self fallbackFamilyNames
		pointSize: self pointSize + anInteger
		stretchValue: self stretchValue
		weightValue: self weightValue
		slantValue: self slantValue 
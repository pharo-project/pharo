visitor
visitLogicalFont: aLogicalFont 
	self newLogicalFontStoredSetting
		fontClassName: aLogicalFont class name;
		familyName: aLogicalFont familyName;
		fallbackFamilyNames: aLogicalFont fallbackFamilyNames;
		pointSize: aLogicalFont pointSize;
		stretchValue: aLogicalFont stretchValue;
		weightValue: aLogicalFont weightValue;
		slantValue: aLogicalFont slantValue.
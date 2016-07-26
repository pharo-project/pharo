tests
testinstallAllFontsIn

	| provider fileInfos families |
	provider := self currentProviderOrError 
		updateEmbeddedFreeTypeFonts;
		yourself.
	fileInfos := provider fileInfosByFamilyAndGroup.
	self assert: fileInfos keys size > 1.
	self assert: (fileInfos includesKey: 'Source Sans Pro').
	self assert: (fileInfos includesKey: 'Source Code Pro').
	families := provider families.
	self assert: (families includesKey: 'Source Sans Pro').
	self assert: (families includesKey: 'Source Code Pro').

character testing
isBigDigit: aCharacter base: anInteger 
	| digitValue |
	digitValue := aCharacter digitValue.
	^ digitValue >= 0
		and: [digitValue < anInteger]
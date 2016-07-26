testing
isItalicOrOblique
	slantValue ifNil:[slantValue := 0].
	^slantValue = 1 or:[slantValue = 2]
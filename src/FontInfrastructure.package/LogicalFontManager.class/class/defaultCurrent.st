instance creation
defaultCurrent
	^self new
		addFontProvider: FreeTypeFontProvider current;
		yourself
action
updateFontList
	FreeTypeFontProvider current updateFromSystem.
	self refreshFontList.
	self changed:#fontListStrings.
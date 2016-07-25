updating
update: aSymbol 
	"Refer to the comment in View|update:."

	aSymbol == self getEnabledSelector ifTrue:
		[self updateEnabled.
		^ self].
	aSymbol == self getTextSelector ifTrue:
		[self updateText.
		^ self]
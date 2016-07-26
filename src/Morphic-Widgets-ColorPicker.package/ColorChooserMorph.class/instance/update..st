updating
update: aSymbol 
	"Refer to the comment in View|update:."

	super update: aSymbol.
	aSymbol == self getEnabledSelector ifTrue: 
		[self updateEnabled.
		^ self]
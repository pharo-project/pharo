updating
update: aSymbol 
	"Refer to the comment in View|update:."

	aSymbol == self getColorSelector ifTrue: 
		[self updateColor.
		^ self]
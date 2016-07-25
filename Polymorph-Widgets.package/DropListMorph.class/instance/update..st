updating
update: aSymbol 
	"Refer to the comment in View|update:."

	aSymbol == getListSelector ifTrue: 
		[self updateList.
		^ self].
	aSymbol == getIndexSelector ifTrue:
		[self updateListSelectionIndex.
		^ self].
	aSymbol == getEnabledSelector ifTrue:
		[self updateEnabled.
		^ self].

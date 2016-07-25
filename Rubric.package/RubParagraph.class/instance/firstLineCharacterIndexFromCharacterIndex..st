querying
firstLineCharacterIndexFromCharacterIndex: anIndex
	^ (self lines at: (self lineIndexOfCharacterIndex: anIndex)) first
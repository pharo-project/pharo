archive operations
addTree: aFileNameOrDirectory removingFirstCharacters: n 
	^ self
		addTree: aFileNameOrDirectory
		removingFirstCharacters: n
		match: [:e | true]
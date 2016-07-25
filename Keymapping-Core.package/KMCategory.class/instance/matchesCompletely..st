testing
matchesCompletely: aString
	^ self keymaps anySatisfy: [ :entry | 
		entry matchesCompletely: aString ]
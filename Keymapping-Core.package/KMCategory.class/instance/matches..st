testing
matches: aString
	^ self keymaps anySatisfy: [ :entry | 
		entry matches: aString ]
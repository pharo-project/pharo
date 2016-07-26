comparing
= aShortcut
	super = aShortcut
		ifFalse: [ ^ false ]. 
	 modifier = aShortcut modifier
		ifFalse: [ ^ false ].
	aShortcut character = character
		ifFalse: [ ^ false ].
	^ true
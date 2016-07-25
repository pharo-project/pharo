comparing
= aShortcut
	super = aShortcut
		ifFalse: [ ^ false ].
	^ aShortcut eventCode = self eventCode
accessing
hasKeyboardFocus
	^ (self scrollPane ifNil: [ ^false ]) hasKeyboardFocus
accessing
resetCurrent
	| tmpCurrent |
	current ifNil: [ ^ self ].
	tmpCurrent := current.
	current := nil.
	FreeTypeFontProvider current removeFontInstaller: tmpCurrent.
*Text-Edition
actOnClick: anEvent for: anObject in: paragraph editor: editor

	| result |
	result := Smalltalk compiler evaluate: evalString for: anObject logged: false.
	UIManager inform: result printString.
	^ true 
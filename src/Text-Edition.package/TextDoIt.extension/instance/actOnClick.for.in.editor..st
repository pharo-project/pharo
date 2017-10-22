*Text-Edition
actOnClick: anEvent for: anObject in: paragraph editor: editor

	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Smalltalk compiler evaluate: evalString for: anObject logged: false.
	^ true 
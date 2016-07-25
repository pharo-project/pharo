event handling
keyStroke: evt 
	"Handle a keystroke event."

	Smalltalk tools 
		codeCompletionAround: [ self basicKeyStroke: evt ] 
		textMorph: self 
		keyStroke: evt.
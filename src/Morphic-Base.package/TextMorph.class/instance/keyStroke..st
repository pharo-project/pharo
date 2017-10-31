event handling
keyStroke: evt 
	"Handle a keystroke event."

	(Smalltalk tools hasToolNamed: #codeCompletion) 
		ifTrue: [ 
			Smalltalk tools 
				codeCompletionAround: [ self basicKeyStroke: evt ] 
				textMorph: self 
				keyStroke: evt ]
		ifFalse: [  self basicKeyStroke: evt ]
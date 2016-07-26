event handling
keyStroke: evt
	"Handle a keystroke. Deal with navigation keys also."
	
	| view |  
	
	editView keyStroke: evt.
	(editView scrollByKeyboard: evt) ifTrue: [^self].
	(acceptOnCR and: [evt keyCharacter = Character cr])
		ifFalse: [(editView navigationKey: evt) ifTrue: [^self]].
	self editor model: editView model.  "For evaluateSelection"
	view := editView.  "Copy into temp for case of a self-mutating doit"
	(acceptOnCR and: [evt keyCharacter = Character cr])
		ifTrue: [^ self editor accept].
	super keyStroke: evt.
	view textChanged.
	view scrollSelectionIntoView.
	self doAutoAccept: evt.
	view scrollSelectionIntoView
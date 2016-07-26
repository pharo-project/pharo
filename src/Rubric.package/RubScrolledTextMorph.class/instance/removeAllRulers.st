rulers handling
removeAllRulers
	[ self rulers notEmpty ] whileTrue: [ self withoutRuler: self rulers first ]
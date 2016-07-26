testing
includesSubCommand: aName
	self withFirstArgument: [ :arg|
		arg = aName ifTrue: [ ^ true ]].
	^ false
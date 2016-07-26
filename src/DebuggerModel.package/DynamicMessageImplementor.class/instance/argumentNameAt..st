evaluating-private
argumentNameAt: anIndex

	| argumentName argument |
	
	argument := message arguments at: anIndex.
	argumentName := argument class canonicalArgumentName.
	
	[ argumentNames includes: argumentName ] whileTrue: [ argumentName := argumentName , anIndex asString ].
	argumentNames add: argumentName.

	^ argumentName
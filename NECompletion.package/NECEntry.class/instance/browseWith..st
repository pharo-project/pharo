operations
browseWith: anECContext 
	| guessType |
	guessType := self guessTypeWith: anECContext.
	guessType ifNil: [^ false].
	Smalltalk tools browser openOnClass: guessType.
	^ true
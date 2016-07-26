operations
guessTypeWith: anECContext 
	| globalEntry |
	globalEntry := Smalltalk at: contents ifAbsent: [^ nil].
	globalEntry isBehavior ifTrue: [^ globalEntry].
	globalEntry ifNotNil: [^ globalEntry class].
	^ nil
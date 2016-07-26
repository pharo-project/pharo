environments
| anEnvironment 
	"If we or anEnvironment includes everything, then return it instead of creating 
	an or that will include everything."

	self isSystem ifTrue: [^self].
	anEnvironment isSystem ifTrue: [^anEnvironment].
	^ RBOrEnvironment onEnvironment: self or: anEnvironment
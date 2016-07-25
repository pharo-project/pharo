environments
& anEnvironment 
	"If we or anEnvironment includes everything, then just include the other environment (optimization)"

	self isSystem ifTrue: [^anEnvironment].
	anEnvironment isSystem ifTrue: [^self].
	^RBAndEnvironment onEnvironment: self and: anEnvironment
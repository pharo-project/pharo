class initialization
initialize
	"ExternalFunction initialize"
	FFIConstants initialize. "ensure proper initialization"
	self initializeErrorMessages.
	(Smalltalk specialObjectsArray at: 47) == self 
		ifFalse:[Smalltalk recreateSpecialObjectsArray].

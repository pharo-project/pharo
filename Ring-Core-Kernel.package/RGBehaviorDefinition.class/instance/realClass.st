accessing
realClass
	"Retrieves the current class existing in the runtime environment"

	^self rootEnvironment classNamed: self name 
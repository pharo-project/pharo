ConfigurationOfAtomicFan
postLoad31baseline
	"Throw an error if GoferFar isn't loaded"

	Smalltalk at: #GoferFar ifAbsent: [ self error: 'GoferFar should be loaded' ]
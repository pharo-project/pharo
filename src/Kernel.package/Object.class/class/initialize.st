class initialization
initialize
	"Object initialize"
	DependentsFields ifNil:[self initializeDependentsFields].
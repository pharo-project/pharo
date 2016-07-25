testing
includesVersionNamed: aString
	"avoid slow default implementation and directly check for existing files"
	^ (self allFileNamesForVersionNamed: aString) isEmpty not
instance creation
isAFileNamed: fName
	"return whether a file exists with the given name"
	^self concreteStream isAFileNamed: (self fullName: fName)
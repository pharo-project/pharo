instance creation
forceNewFileNamed: fileName
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, delete it without asking before creating the new file."

	^self concreteStream forceNewFileNamed: fileName
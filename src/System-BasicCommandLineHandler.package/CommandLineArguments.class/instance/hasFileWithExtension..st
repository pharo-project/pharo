testing
hasFileWithExtension: aFileExtension
	"return true if the first argument has the given file extension"
	^ self arguments anySatisfy: [ :arg| 
		arg endsWith: aFileExtension].
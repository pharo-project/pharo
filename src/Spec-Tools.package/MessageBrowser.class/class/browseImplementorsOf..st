instance creation
browseImplementorsOf: aSymbol 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self browse: aSymbol implementors asImplementorsOf: aSymbol
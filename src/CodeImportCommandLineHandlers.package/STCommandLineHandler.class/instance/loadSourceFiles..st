loading
loadSourceFiles: anArray
	"Load all the source files in the given array."

	sourceFiles := anArray collect: [ :each | 
						(FileSystem disk resolve: each) asFileReference ].

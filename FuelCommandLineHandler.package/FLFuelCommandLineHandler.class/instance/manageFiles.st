manage
manageFiles
	"Load all the source files in the given array."
	files := (self commandLine allFilesWithExtension: self fileExtension)
		 collect: [ :each | (FileSystem disk resolve: each) asFileReference ].
	files ifNil: [ ^self ].
	files do: [ :reference | self manageFile: reference ] 

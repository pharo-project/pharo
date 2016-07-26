validation
create
	"create me in the FT2Plugin. This gets my handle, and loads the fields"
	
	fileContentsExternalMemory 
		ifNotNil: [ self newFaceFromExternalMemory: fileContentsExternalMemory index: index ]	
		ifNil: [	self newFaceFromFile: filename asFileReference index: index ].
	self loadFields

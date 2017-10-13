open/close
basicOpenForWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	^ self class
		retryWithGC: [ self primOpen: name writable: writeMode ]
		until:[ :id | id notNil ] 
		forFileNamed: name
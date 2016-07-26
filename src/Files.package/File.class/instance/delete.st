open/close
delete
	"We retries with GC because in some platforms open files cannot be open"
	self class
		retryWithGC: [ self primDelete: name ]
		until: [ :result | result notNil ]
		forFileNamed: name.
	self exists
		ifTrue: [ (CannotDeleteFileException new messageText: 'Could not delete file ' , name,'. Check the file is not open.') signal ].
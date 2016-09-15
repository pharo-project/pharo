private
writeEntry: anEntry ifSuccess: aBlock

	self ensureInitializedForWriting.
	
	"It might happen that two threads log a new entry concurrently..."
	writerLock critical: [ [ 

		| newPosition |
		"TODO: It is slow to open and close the file for each entry..."
		fileReference writeStreamDo: [ :aWriteStream | 
			aWriteStream setToEnd.
			newPosition := aWriteStream position.

			"write entry"
			entryWriter 
				on: aWriteStream 
				nextEntryPut: anEntry.
				
			"In Linux it was necessary to explicitely flush the stream"
			aWriteStream flush ].

			"--> Success <--"		
			aBlock value: newPosition. 
			
			]	on: FileException, FileSystemError
				do: [ :error | self writingError: error asString ]

	] ifLocked: [
		"This is a escape valve to avoid a deadlock, e.g. while debugging an error inside above block."		
		self writingError: 'this store is locked' ]

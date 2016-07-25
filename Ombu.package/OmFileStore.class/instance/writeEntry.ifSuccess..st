private
writeEntry: anEntry ifSuccess: aBlock

	self ensureInitializedForWriting.

	"It might happen that two threads log a new entry concurrently..."
	writerLock critical: [ [ 

		| newPosition |
		self flag: #todo. "It is slow to open and close the file for each entry..."
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
			
			]	on: Error 
				do: [ :error | self inform: 'Ombu: Couldn''t write entry, since ', error asString ]

	] ifLocked: [
		"This is a escape valve to avoid a deadlock, e.g. while debugging an error inside above block."		
		self inform: 'Ombu: Couldn''t write entry since this store is locked.' ]

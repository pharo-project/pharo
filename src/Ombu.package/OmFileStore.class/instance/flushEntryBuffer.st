private
flushEntryBuffer

	self critical: [
		| initialPosition initialLocalName |
		self entryBuffer isEmpty ifTrue: [ ^self ].
		
		fileReference writeStreamDo: [:fileStream |
			fileStream setToEnd.
			
			initialPosition := fileStream position.
			initialLocalName := self entryBuffer first value.
		
			ZnBufferedWriteStream on: fileStream do: [:aWriteStream |
				| anEntryWriter |
				anEntryWriter := self newEntryWriter.

				[ self entryBuffer isEmpty ] whileFalse: [
					| next entry |
					next := self entryBuffer removeFirst.
					entry := next key.

					"Write entry to file"
					anEntryWriter
						on: aWriteStream 
						nextEntryPut: entry.
					].
				].

			"In Linux it was necessary to explicitely flush the file stream"
			fileStream flush.
			].
	
		self refreshEntryPositionsByLocalNameStartingAt: initialPosition since: initialLocalName.
		].

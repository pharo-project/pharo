private
flushEntryBuffer

	self critical: [
		| anEntryWriter |
		anEntryWriter := self newEntryWriter.

		fileReference writeStreamDo: [:fileStream | 
			fileStream setToEnd.			
			ZnBufferedWriteStream on: fileStream do: [ :aWriteStream |
				
				[ self entryBuffer isEmpty ] whileFalse: [
					| next entry localName newPosition |
					next := self entryBuffer removeFirst.
					entry := next key.
					localName := next value.
					newPosition := aWriteStream position.

					"Write entry to file"
					anEntryWriter 
						on: aWriteStream 
						nextEntryPut: entry.

					entryPositionsByLocalName at: localName put: newPosition
					].
				].

			"In Linux it was necessary to explicitely flush the file stream"
			fileStream flush.
		] ]

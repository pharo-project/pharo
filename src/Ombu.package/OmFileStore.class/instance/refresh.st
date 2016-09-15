refreshing
refresh

	| entry |
	self initialize.
	
	self fileReference exists ifFalse: [ ^self ].

	self readEntriesWith: [ :readStream | | entryId |
		entryId := 0.
		[ readStream atEnd ] whileFalse: [ | filePosition entryPosition |

			[ filePosition := readStream position.
			entryPosition := self entryReader moveStreamToNextEntry: readStream.
			readStream atEnd ifFalse: [ 
				entryPositionsByLocalName 
					at: (entryId := entryId + 1) asString
					put: entryPosition]
			] on: Error do: [ :error | 
				self 
					readingError: error
					on: self fileReference
					position: filePosition ] ].
		entryCount := entryPositionsByLocalName size.
		readStream position: (entryPositionsByLocalName at: entryId asString ifAbsent: [^self]).
		entry := self entryReader nextEntryFrom: readStream.
		headReference := self referenceTo: entry
	]
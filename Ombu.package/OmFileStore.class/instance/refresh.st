refreshing
refresh

	| entry |
	self initialize.
	
	self fileReference exists ifFalse: [ ^self ].

	self readEntriesWith: [ :entryReader :readStream | | entryId |
		entryId := 0.
		[ readStream atEnd ] whileFalse: [ | filePosition entryPosition |

			[ filePosition := readStream position.
			entryPosition := entryReader moveStreamToNextEntry: readStream.
			readStream atEnd ifFalse: [ 
				entryPositionsByLocalName 
					at: (entryId := entryId + 1) asString
					put: entryPosition]
			] on: Error do: [ :error | 
				self 
					error: error
					reading: self fileReference
					position: filePosition ] ].
		entryCount := entryPositionsByLocalName size.
		readStream position: (entryPositionsByLocalName at: entryId asString ifAbsent: [^self]).
		entry := entryReader nextEntryFrom: readStream.
		headReference := self referenceTo: entry
	]
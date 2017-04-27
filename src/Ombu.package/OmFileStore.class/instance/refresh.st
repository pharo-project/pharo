refreshing
refresh

	self flush.

	self critical: [
		mustRefresh := false.
		self initialize.
		self fileReference exists ifFalse: [ ^self ].
		self readEntriesWith: [:readStream | [
			self newEntryReader
				stream: readStream;
				entryPositionsDo: [ :entryPosition |
					entryCount := entryCount + 1.
					entryPositionsByLocalName at: entryCount asString put: entryPosition ]
			] ensure: [
				headReference := self referenceToLocalName: entryCount asString ]
			]
		]
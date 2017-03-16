refreshing
refresh

	mustRefresh := false.
	self flush.

	self critical: [
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
refreshing
refresh

	self flush.

	self critical: [
		mustRefresh := false.
		self initialize.
		self fileReference exists ifFalse: [ ^self ].
		self readEntriesWith: [:readStream |
			self refreshNewBlocksFrom: readStream.
			self refreshHeadReferenceAndEntryCountFrom: readStream. ]
		]
	
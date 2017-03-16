private
refreshEntryPositionsByLocalNameStartingAt: firstStreamPosition since: initialLocalName
	"Workaround needed because can't get real file stream position from ZnBufferedWriteStream. 
	(+ would need special care of WideStrings)"

	| localNameAsInteger |
	localNameAsInteger := initialLocalName asInteger.

	self readEntriesWith: [:readStream |
		readStream position: firstStreamPosition.
		self newEntryReader
			stream: readStream;
			entryPositionsDo: [ :entryPosition |
				entryPositionsByLocalName at: localNameAsInteger asString put: entryPosition.
				localNameAsInteger := localNameAsInteger + 1 ].
		].
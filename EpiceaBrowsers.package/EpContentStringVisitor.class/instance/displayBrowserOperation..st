private
displayBrowserOperation: aBrowserOperationEvent

	^ String streamContents: [ :stream |
		stream nextPutAll: '('.
		stream nextPutAll: aBrowserOperationEvent title.
		stream nextPutAll: ') ('.
		aBrowserOperationEvent entryReferences size printOn: stream.
		stream nextPutAll: ' entries)' ]
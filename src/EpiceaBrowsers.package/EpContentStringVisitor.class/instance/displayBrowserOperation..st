private
displayBrowserOperation: aBrowserOperationEvent

	^ String streamContents: [ :stream |
		stream nextPutAll: 'Triggered '.
		aBrowserOperationEvent entryReferences size printOn: stream.
		stream nextPutAll: ' code changes' ]
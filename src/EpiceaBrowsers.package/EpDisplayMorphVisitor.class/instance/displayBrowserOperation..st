private
displayBrowserOperation: aBrowserOperationEvent

	^ self displayOperation: (
		String streamContents: [ :stream |
			stream nextPutAll: aBrowserOperationEvent title.
"			stream nextPutAll: ': '.
			aBrowserOperationEvent entryReferences size printOn: stream.
			stream nextPutAll: ' entries'" ])
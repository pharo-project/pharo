private
displayBrowserOperation: aBrowserOperationEvent

	^ self displayOperation: (
		String streamContents: [ :stream |
			stream nextPutAll: aBrowserOperationEvent title ])
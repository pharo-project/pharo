private
displayBrowserOperation: aBrowserOperationEvent

	^ String streamContents: [ :stream |
		stream 
			print: aBrowserOperationEvent entryReferences size;
			nextPutAll: ' code changes produced' ]
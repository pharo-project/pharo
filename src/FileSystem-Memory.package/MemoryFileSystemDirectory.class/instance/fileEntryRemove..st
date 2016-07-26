accessing
fileEntryRemove: aFileName
	^ self fileEntryRemove: aFileName ifAbsent: [ FileDoesNotExist signalWith: aFileName ]
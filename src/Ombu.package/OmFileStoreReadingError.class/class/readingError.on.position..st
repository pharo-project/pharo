instance creation
readingError: sourceError on: aFileReference position: aFilePosition

	^ self new messageText: 'Reading entry from ', aFileReference asString, '@', aFilePosition asString, ': ', sourceError messageText 
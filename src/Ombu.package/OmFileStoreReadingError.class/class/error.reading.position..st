instance creation
error: sourceError reading: aFileReference position: aFilePosition

	^ self new messageText: 'Reading entry from ', aFileReference asString, '@', aFilePosition asString, ': ', sourceError messageText 
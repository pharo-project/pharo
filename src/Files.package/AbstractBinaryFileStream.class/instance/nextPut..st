writing
nextPut: anInteger

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
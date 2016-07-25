instance creation
chunkSize: anInteger
	^ self new action: #chunkSize:; arguments: (Array with: anInteger)
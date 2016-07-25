instance creation
pageSize: anInteger
	^ self new action: #pageSize:; arguments: (Array with: anInteger)
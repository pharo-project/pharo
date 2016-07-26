protocol
removeLastEntry

	self isPaused ifTrue: [ ^ self ].
	storage ifEmpty: [ ^ self ].
	
	storage remove: storage last.	
	index := index -1
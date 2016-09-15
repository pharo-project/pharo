private
createNewActiveLog
	
	| fileNameOrNil sessionStore |
	sessionStore := self monitor sessionStore.

	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension) for the new log.'
		initialAnswer: sessionStore randomName.
	fileNameOrNil ifNil: [ ^ self ].

	self monitor sessionStore resetWithStoreNamed: fileNameOrNil.
	self refresh.
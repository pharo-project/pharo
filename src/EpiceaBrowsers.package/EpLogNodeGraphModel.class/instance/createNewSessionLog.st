private
createNewSessionLog
	
	| fileNameOrNil sessionStore |
	sessionStore := self monitor sessionStore.

	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension) for the new log.'
		initialAnswer: sessionStore nextStoreName.
	fileNameOrNil ifNil: [ ^ self ].

	(self monitor sessionStore existsStoreNamed: fileNameOrNil)
		ifTrue: [ 
			self inform: 'Invalid name: Already used' ]
		ifFalse: [
			self monitor sessionStore resetWithStoreNamed: fileNameOrNil.
			self refresh ]
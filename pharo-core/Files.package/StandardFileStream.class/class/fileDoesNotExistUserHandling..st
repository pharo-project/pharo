error handling
fileDoesNotExistUserHandling: fullFileName
	| selection newName |
	selection := UIManager default
		chooseFrom:
			{('create a new file' translated).
			('choose another name' translated).
			('cancel' translated)}
		title: fullFileName asFileReference basename , ' does not exist.'.
	selection = 1
		ifTrue: [ ^ self new open: fullFileName forWrite: true ].
	selection = 2
		ifFalse: [ ^ self ].
	newName := UIManager default request: 'Enter a new file name' initialAnswer: fullFileName.
	^ self oldFileNamed: (self fullName: newName)
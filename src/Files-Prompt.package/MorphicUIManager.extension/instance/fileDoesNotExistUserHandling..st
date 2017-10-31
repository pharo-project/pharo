*Files-Prompt
fileDoesNotExistUserHandling: filename
	| selection newName |
	selection := self
		chooseFrom:
			{('create a new file' translated).
			('choose another name' translated).
			('cancel' translated)}
		title: filename, ' does not exist.'.
	selection = 1
		ifTrue: [ ^ File openForWriteFileNamed: filename ].
	selection = 2
		ifFalse: [ ^ self ].
	newName := self request: 'Enter a new file name' initialAnswer: filename.
	^ File openForWriteFileNamed: filename
examples
grabScreenAndSaveOnDisk
	"GIFReadWriter grabScreenAndSaveOnDisk"
	| form fileName |
	
	form := Form fromUser.
	form bits size = 0 ifTrue: [ ^ self inform: 'Empty region selected.' ].
	
	fileName := (FileSystem disk workingDirectory / 'Pharo', 'gif') fullName.
		
	UIManager default 
		informUser: 'Writing ' translated, fileName
		during: 
			[ GIFReadWriter 
				putForm: form
				onFileNamed: fileName ]
*Graphics-Files-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>

	^((ImageReadWriter allTypicalFileExtensions add: '*'; add: 'form'; yourself)
		includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [#()]

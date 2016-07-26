System-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>

	(fullName endsWith: self fileSuffix)
		ifFalse: [ ^ #() ].

	^ { self service }
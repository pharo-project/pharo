*System-Changes-FileServices
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	
	^ (FileStream isSourceFileSuffix: suffix)
		ifTrue: [ self services]
		ifFalse: [#()]
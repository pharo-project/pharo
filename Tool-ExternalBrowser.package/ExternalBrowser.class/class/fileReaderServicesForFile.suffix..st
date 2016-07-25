System-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>

	^ (#('st' 'cs') includes: suffix)
		ifTrue: [ Array with: self serviceBrowseCode]
		ifFalse: [ #() ].
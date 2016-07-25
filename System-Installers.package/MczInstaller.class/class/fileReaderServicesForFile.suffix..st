System-FileRegistry
fileReaderServicesForFile: fileName suffix: suffix
	<fileService>
	^({ self extension. '*' } includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [#()].

file service
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	| services |
	services := OrderedCollection new.
	(FileStream isSourceFileSuffix: suffix) | 
	(FileStream isChangesFileSuffix: suffix) | (suffix = '*')
		ifTrue: [ services add: self serviceBrowseCSOrSTFile ].
	^services
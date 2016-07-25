*System-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	| services |
	suffix = 'gz' | (suffix = '*')
		ifFalse: [ ^ #() ].
	services := OrderedCollection new.
	suffix = '*' | (fullName asLowercase endsWith: '.cs.gz') | (fullName asLowercase endsWith: '.mcs.gz')
		ifTrue: [ 
			services add: self serviceFileIn.
			(Smalltalk globals includesKey: #ChangeSorter)
				ifTrue: [ services add: self serviceFileIntoNewChangeSet ] ].
	services addAll: self services.
	^ services
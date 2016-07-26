accessing
configurationVersion
	^ (self hasOption: 'install')
		ifFalse: [ #stable ]
		ifTrue: [ self optionAt: 'install' ]
private
getConfigurationVersion
	| workingCopy |
	workingCopy := MCWorkingCopy registry
		detect: [ :wc | wc packageName = 'ConfigurationOfFuel' ]
		ifNone: [ ^ '-1' ].
	^ workingCopy ancestry ancestors
		ifNotEmpty: [ :ancestors | ancestors first name copyAfterLast: $. ]
		ifEmpty: [ '-1' ]
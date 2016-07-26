private
uniqueVersionNameIn: aRepository
	|versionName|
	counter := nil.
	'Creating unique version number' 
		displayProgressFrom: 0 
		to: 1 
		during: [ :arg|
			[versionName := self nextVersionName.
			aRepository includesVersionNamed: versionName] whileTrue ].
	^ versionName
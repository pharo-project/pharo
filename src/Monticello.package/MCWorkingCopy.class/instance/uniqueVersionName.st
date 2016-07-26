private
uniqueVersionName
	|versionName|
	counter := nil.
	'Creating unique version number' 
		displayProgressFrom: 0 
		to: 1 
		during: [ :arg|
			[versionName := self nextVersionName.
			self repositoryGroup includesVersionNamed: versionName] whileTrue ].
	^ versionName
validation
validate
	| toolBox |
	
	[	toolBox := MetacelloToolBox configurationNamed: self basename.
		toolBox validateVersion: #development ]
	on: MetacelloValidationFailure do: [ :e | 
		VSCommitProjectValidationError new
			projectName: self basename;
			issues: e issues;
			messageText: self basename, ': ', (e issues first explanation);
			signal ].
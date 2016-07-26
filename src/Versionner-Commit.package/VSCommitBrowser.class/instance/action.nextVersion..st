actions
action: action nextVersion: value
	self 
		assert: action isProject 
		description: 'Not a project!'. 
	VSCommitVersion
		parseSemanticVersionFor: value 
		ifError: [ self error: 'Not a semantic version number!' ].		

	action nextVersion: value
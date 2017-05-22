accessing
config
	| config |
	config := (LGitConfig of: self) autoRelease.
	self withReturnHandlerDo: [ self repository_config: config ].
	^ config

load primitives
loadConfiguration: configurationName version: version
	[ Gofer it 
		repository: self inboxRepositoryDefault;
		configurationOf: configurationName;
		loadVersion: version ]
	on: Warning do: [ :w | w resume: true ]
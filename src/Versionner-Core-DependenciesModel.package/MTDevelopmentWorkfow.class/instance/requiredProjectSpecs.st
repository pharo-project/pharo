private
requiredProjectSpecs
	^ project requiredProjects 
		collect: [ :requiredProject |  VersionnerToolBox projectSpecFromRequiredProject: requiredProject ].
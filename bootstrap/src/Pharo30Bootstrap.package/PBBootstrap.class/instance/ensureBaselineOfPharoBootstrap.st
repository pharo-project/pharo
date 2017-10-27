pre-requisites
ensureBaselineOfPharoBootstrap
	
	(self originRepository versionWithInfo: 
		(self originRepository versionInfoFromVersionNamed: 'Metacello-PharoExtensions'))
			load.
	(self originRepository versionWithInfo: 
		(self originRepository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap'))
			load.

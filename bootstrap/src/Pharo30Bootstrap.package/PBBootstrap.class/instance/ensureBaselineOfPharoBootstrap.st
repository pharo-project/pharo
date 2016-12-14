pre-requisites
ensureBaselineOfPharoBootstrap
	
	(self originRepository versionWithInfo: 
		(self originRepository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap'))
			load
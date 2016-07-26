versionInfo
ancestorsFor: packageSpec

	^self loadData
		ancestorsFor: packageSpec 
		ifAbsent: [ super ancestorsFor: packageSpec ]
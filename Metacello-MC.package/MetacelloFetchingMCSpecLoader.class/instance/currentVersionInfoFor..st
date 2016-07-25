versionInfo
currentVersionInfoFor: packageSpec

	^self loadData
		currentVersionInfoFor: packageSpec 
		ifAbsent: [ super currentVersionInfoFor: packageSpec ]
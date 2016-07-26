versionInfo
currentVersionInfoFor: packageSpec

	^self loadData
		currentVersionInfoFor: packageSpec 
		ifAbsent: [ nil ]
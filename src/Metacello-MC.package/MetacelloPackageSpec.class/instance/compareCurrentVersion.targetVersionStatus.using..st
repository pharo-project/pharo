testing
compareCurrentVersion: anOperator targetVersionStatus: statusIgnored using: anMCLoader

	self 
		currentPackageLoaded: [:bool | ^bool ] 
		comparing: anOperator 
		notLoaded: [:ignored | ^false ]
		using: anMCLoader
accessing
rPackage
	^ packageAnalysis rPackageSet packages detect: [ :each | each packageName = self packageName ]
mocks
mockVersion

	^ MCVersion 
		package: self mockPackage
		info: self mockVersionInfo
		snapshot: MCSnapshot empty
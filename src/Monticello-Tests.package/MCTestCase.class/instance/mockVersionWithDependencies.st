mocks
mockVersionWithDependencies
	^ MCVersion 
		package: self mockPackage
		info: self mockVersionInfo
		snapshot: self mockSnapshot
		dependencies: self mockDependencies
mocks
mockVersionWithAncestor: aMCVersion 
	^ MCVersion
		package: self mockPackage
		info: (self mockVersionInfoWithAncestor: aMCVersion info)
		snapshot: self mockSnapshot
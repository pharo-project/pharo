preparation
exportPackages: packageList usingInitializeScript: aScriptString intoFile: aFileName

	self
		exportPackages: packageList
		withBlacklistedClasses: #()
		withBlacklistedMethods: #() 
		usingInitializeScript: aScriptString
		intoFile: aFileName
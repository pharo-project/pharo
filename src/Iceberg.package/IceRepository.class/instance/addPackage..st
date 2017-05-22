actions
addPackage: aWorkingCopy
	^ self savedPackagesDictionary
		at: aWorkingCopy packageName
		ifAbsentPut: [
			IceSavedPackage 
				named: aWorkingCopy packageName 
				repository: self ]
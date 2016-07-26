enumerating
projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock

	self packageSpecsInLoadOrder do: [:pkgSpec |
		pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
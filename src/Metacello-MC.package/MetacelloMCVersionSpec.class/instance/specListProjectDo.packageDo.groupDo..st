enumerating
specListProjectDo: projectBlock packageDo: packageBlock groupDo: groupBlock

	self packages specListDo: [:pkgSpec |
		pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
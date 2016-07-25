testing
canMakePackage

	^ self issueNumber isNotNil
		and: [title isEmptyOrNil not
		and: [self includedPackages isEmptyOrNil not]]
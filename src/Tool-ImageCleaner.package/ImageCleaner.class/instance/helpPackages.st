cleaning
helpPackages
	^RPackageOrganizer default packageNames select: [ :each | each endsWith: '-Help' ]
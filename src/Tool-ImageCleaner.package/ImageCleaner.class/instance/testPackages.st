cleaning
testPackages
	^(RPackageOrganizer default  packageNames select: [ :each | each endsWith: 'Tests' ]) copyWithout: 'ReleaseTests'
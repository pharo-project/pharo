tests
testspotterExtensionTitleOf
	
	| t |
	t := GTSpotterExtensionSettings spotterExtensionTitleOf: MCPackage>> #spotterForPackageFor:.
	self assert: t equals: 'Packages'
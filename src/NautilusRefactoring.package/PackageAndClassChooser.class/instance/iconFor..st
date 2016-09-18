accessing - tree
iconFor: anRPackageOrClass
	anRPackageOrClass isBehavior
		ifTrue: [ ^ self iconNamed: #classIcon ].
	RPackageOrganizer default
		packageNamed: anRPackageOrClass name
		ifAbsent: [ ^ self iconNamed: #emptyPackageIcon ].
	^ self iconNamed: #packageIcon
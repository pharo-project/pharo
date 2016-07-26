accessing - tree
iconFor: anRPackageOrClass
	anRPackageOrClass isBehavior
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #classIcon ].
	RPackageOrganizer default
		packageNamed: anRPackageOrClass name
		ifAbsent: [ ^ Smalltalk ui icons iconNamed: #emptyPackageIcon ].
	^ Smalltalk ui icons iconNamed: #packageIcon
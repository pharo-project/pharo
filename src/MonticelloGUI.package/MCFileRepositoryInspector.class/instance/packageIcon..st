morphic ui
packageIcon: aPackage
	| p |
	newer ifNil: [ newer := #() ].
	(loaded anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifFalse: [ ^ Smalltalk ui icons iconNamed: #emptyPackageIcon ].

	"If newer collection includes our package it means we have an older mcz version loaded"
	(newer anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #newerPackagesAvailableIcon ].
	p := RPackageOrganizer default
		packageNamed: aPackage
		ifAbsent: [ nil ].
	p ifNil: [ ^ Smalltalk ui icons iconNamed: #emptyPackageIcon ].
	^ p isDirty
		ifFalse: [ Smalltalk ui icons iconNamed: #packageIcon ]
		ifTrue: [ Smalltalk ui icons iconNamed: #dirtyPackageIcon ]
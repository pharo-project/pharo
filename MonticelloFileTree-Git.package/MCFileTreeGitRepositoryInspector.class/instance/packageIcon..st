morphic ui
packageIcon: aPackage
	| p |
	newer ifNil: [ newer := #() ].
	(loaded anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifFalse: [ ^ Smalltalk ui icons emptyPackageIcon ].
		"If newer collection includes our package it means we have an older mcz version loaded"
	(newer anySatisfy: [ :each | each info name beginsWith: aPackage , '-' ])
		ifTrue: [ ^ Smalltalk ui icons newerPackagesAvailableIcon ].
	p := RPackageOrganizer default packageNamed: aPackage ifAbsent: [ nil ].
	p ifNil: [ ^ Smalltalk ui icons emptyPackageIcon ].
	^ p isDirty
		ifFalse: [ Smalltalk ui icons packageIcon ]
		ifTrue: [ Smalltalk ui icons dirtyPackageIcon ]
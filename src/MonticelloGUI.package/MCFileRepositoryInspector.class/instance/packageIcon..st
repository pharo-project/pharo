morphic ui
packageIcon: aPackage
	| p |
	newer ifNil: [ newer := #() ].
	(loaded anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifFalse: [ ^ self iconNamed: #emptyPackageIcon ].

	"If newer collection includes our package it means we have an older mcz version loaded"
	(newer anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifTrue: [ ^ self iconNamed: #newerPackagesAvailableIcon ].
	p := RPackageOrganizer default
		packageNamed: aPackage
		ifAbsent: [ nil ].
	p ifNil: [ ^ self iconNamed: #emptyPackageIcon ].
	^ p isDirty
		ifFalse: [ self iconNamed: #packageIcon ]
		ifTrue: [ self iconNamed: #dirtyPackageIcon ]
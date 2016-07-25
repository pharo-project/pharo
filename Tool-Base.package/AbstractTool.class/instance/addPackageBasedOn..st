package
addPackageBasedOn: aPackage

	| name initialAnswer package |
	initialAnswer := aPackage ifNil: [''] ifNotNil: [:p | p name].
	name := UIManager default request: 'Name of the new package:' initialAnswer: initialAnswer title: 'Create a new package'.
	(self isValidPackageName: name)
		ifTrue: [ package := self browsedEnvironment packageOrganizer createPackageNamed: name ]
		ifFalse: [ self alertInvalidPackageName:name ].
	^ package
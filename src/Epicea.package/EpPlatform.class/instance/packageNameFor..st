convenience
packageNameFor: aClassOrTrait

	^ (RPackage organizer packageOfClassNamed: aClassOrTrait name)
		ifNotNil: [ :package | package name ]
		ifNil: [ 'nil' ]
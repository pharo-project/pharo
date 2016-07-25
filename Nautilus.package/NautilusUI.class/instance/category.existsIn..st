package
category: aClassTag existsIn: aPackage
	aPackage classTagNamed: aClassTag ifPresent:[^ true].
	^ false.
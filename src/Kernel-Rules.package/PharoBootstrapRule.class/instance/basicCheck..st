running
basicCheck: aPackage
	(self bootstrapPackages includes: aPackage packageName)
		ifFalse: [ ^ false "Do not check" ].
	^ self newExternalDependencyExistFor: aPackage
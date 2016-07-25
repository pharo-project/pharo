updating
refreshPackageDependencies: aPackage
	aPackage clearDependencies.
	self computeStaticDependencies: aPackage.
	self combineDependencies: aPackage.
	aPackage removeAllInternal.
	aPackage beSeen.
	
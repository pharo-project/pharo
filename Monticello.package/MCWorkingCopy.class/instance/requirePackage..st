accessing
requirePackage: aPackage
	(self requiredPackages includes: aPackage) ifFalse: [requiredPackages add: aPackage]
protocol
remove: aPackage from: aGroup

	^ aGroup
		ifAllowed: [ aGroup removePackages: { aPackage } ]
		ifNot: [ self openReadOnlyError ]
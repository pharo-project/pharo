adding/removing
addPackageNamed: packageName
	"adds a child package with the given name"

	(self includesPackageNamed: packageName) ifTrue: [ ^self ].
	self addPackage: (RGPackage named: packageName)
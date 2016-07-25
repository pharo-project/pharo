utility
createNewPackageNamed: aName

	| pack |
	self removePackage: aName.
	pack := self packageClass named: aName.
	createdPackages add: pack.
	^ pack
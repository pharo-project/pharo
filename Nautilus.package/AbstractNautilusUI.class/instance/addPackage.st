menus behavior
addPackage

	| package |
	package := self addPackageBasedOn: self selectedPackage.
	package ifNotNil: [
			self selectedPackage: package.
			self updatePackageViewAndMove ]
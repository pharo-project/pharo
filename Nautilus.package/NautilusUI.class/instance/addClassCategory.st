menus behavior
addClassCategory 
	| package |

	package := self addClassCategoryBasedOn: self selectedPackage.
	package ifNotNil: [
		self selectedPackage: package.
		self updatePackageView ]
menus behavior
renamePackage

	self selectedPackages do: [ :package | self renamePackage: package ].
	self updatePackageView.
	
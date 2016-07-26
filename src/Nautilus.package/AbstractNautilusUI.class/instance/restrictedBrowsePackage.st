menus behavior
restrictedBrowsePackage

	self selectedPackages
		ifEmpty: [ ^ self ]
		ifNotEmpty: [ :packages |
			^ self restrictedBrowsePackages: packages ]
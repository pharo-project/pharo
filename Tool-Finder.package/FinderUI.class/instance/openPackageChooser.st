private
openPackageChooser
	(DialogItemsChooser
		on: self
		unselectedItems: self constructPackagesSet
		selectedItems: (self constructPackagesSet: self packagesSelection)
		selectedItemsSetterSelector: #collectFromPackages: 
		title: 'Package Chooser'
		unselectedItemsLabel: 'Unselected Packages' translated
		selectedItemsLabel: 'Selected Packages' translated) open
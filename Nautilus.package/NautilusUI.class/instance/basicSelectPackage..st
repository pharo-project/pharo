accessing
basicSelectPackage: aPackage
	| selection |
			
	selection := aPackage asNautilusSelection.
	self giveFocusTo: self packageWidget.
	self selectedPackageWithoutChangingSelection: selection.
	self listSelectItem: selection.
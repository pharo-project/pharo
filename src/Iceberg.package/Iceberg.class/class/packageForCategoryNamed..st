accessing
packageForCategoryNamed: categoryName
	| systemPackage |
	systemPackage := RPackageOrganizer default packageMatchingExtensionName: categoryName.
	^ self packageNamed: systemPackage name

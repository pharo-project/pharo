widget package
buildPackageWidget
	| packagesList |
	packagesList := packageWidget buildPackagesList.
	self setShortcuts: #NautilusPackageShortcuts to: packagesList.
	^ packagesList
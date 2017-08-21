running
runPackage: aString
	| packageEnvironment |
	packageEnvironment := RBBrowserEnvironment new
		forPackageNames: (Array with: aString).
	packageEnvironment label: aString.
	^ self runEnvironment: packageEnvironment
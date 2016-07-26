private
manuallyResolvedDependenciesOf: aPackageName
	^ (RPackage organizer packageNamed: aPackageName) manuallyResolvedDependencies 
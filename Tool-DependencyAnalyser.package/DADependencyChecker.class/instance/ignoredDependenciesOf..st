private
ignoredDependenciesOf: aPackageName
	^ (RPackage organizer packageNamed: aPackageName) ignoredDependencies 
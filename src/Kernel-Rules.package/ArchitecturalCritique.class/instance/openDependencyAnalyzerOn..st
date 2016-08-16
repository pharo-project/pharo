actions
openDependencyAnalyzerOn: package

	^ (DAPackageAnalyzerPackageDependenciesWindow
			onPackagesNamed: {package packageName}) open
computing
missingMethodsImplementedIn: aPackageName
	^ (RPackageOrganizer default packageNamed: aPackageName) selectors
		intersection: self missingMethods
private
computeDependenciesOf: aPackageName
	| dependencies |
	dependencies := (self daPackageFor: aPackageName) dependentPackages collect: #packageName.
	^ ((dependencies
			addAll: (self manuallyResolvedDependenciesOf: aPackageName);
			yourself) 
		difference: (self ignoredDependenciesOf: aPackageName)) 
		sorted
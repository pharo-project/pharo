converting
dependencyToArray: aDependency
	^ {
		aDependency package name . 
		aDependency versionInfo name . 
		aDependency versionInfo id asString }
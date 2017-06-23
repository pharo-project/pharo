versions
versionFrom: aVersionName 
	| packageName package version |
	packageName := (MCFileTreeRepository parseName: aVersionName) first.
	package := self repository packageNamed: packageName.
	
	version := package latestVersion.
	version info name = aVersionName ifFalse: [
		version := package versions detect: [ :ver | ver info name = aVersionName ]].
	
	^ version mcVersion
	
	
	
	
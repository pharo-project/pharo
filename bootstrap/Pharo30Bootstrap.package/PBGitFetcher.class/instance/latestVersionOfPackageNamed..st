helpers
latestVersionOfPackageNamed: aString 

	| ftRepo |
	"(repository packageNamed: aString ) latestVersion mcVersion"
	ftRepo := (MCFileTreeRepository new directory: 'src' asFileReference).
	^ (ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: aString )).
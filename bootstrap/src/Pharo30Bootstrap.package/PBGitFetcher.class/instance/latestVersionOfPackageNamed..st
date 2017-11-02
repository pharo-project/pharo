helpers
latestVersionOfPackageNamed: aString 

	| ftRepo |
	"(repository packageNamed: aString ) latestVersion mcVersion"
	ftRepo := (TonelRepository new directory: 'src' asFileReference).
	^ (ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: aString )).
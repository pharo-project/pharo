as yet unclassified
ensureRepository

	| ftRepo |
	"repository ensureLocalRepository."
	ftRepo := (MCFileTreeRepository new directory: 'src' asFileReference).
	(ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap' )) load.
		
"		packageNamed: 'BaselineOfPharoBootstrap') loadLatest."
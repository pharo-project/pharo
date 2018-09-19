as yet unclassified
ensureRepository

	| ftRepo |
	"repository ensureLocalRepository."
	ftRepo := (TonelRepository new directory: 'src' asFileReference).
	(ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap' )) load.
		
"		packageNamed: 'BaselineOfPharoBootstrap') loadLatest."
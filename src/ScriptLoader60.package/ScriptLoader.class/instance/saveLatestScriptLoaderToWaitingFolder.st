elementary steps
saveLatestScriptLoaderToWaitingFolder
	"self new saveLatestScriptLoaderToWaitingFolder"

	| man r |
	man := self allManagers.
	r := man select: [ :each | each packageName sameAs: self theScriptLoaderPackageName ].
	self
		saveInToReloadCachePackage: r first
		with: self commentForCurrentUpdate
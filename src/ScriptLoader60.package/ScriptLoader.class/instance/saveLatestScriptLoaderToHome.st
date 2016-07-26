elementary steps
saveLatestScriptLoaderToHome
	"self new saveLatestScriptLoaderToHome"

	| man r |
	man := self allManagers.
	r := man select: [:each | each packageName matchesRegex: self theScriptLoaderPackageName, '.*' ].
	self repository storeVersion: (r first newVersionIn: self repository).
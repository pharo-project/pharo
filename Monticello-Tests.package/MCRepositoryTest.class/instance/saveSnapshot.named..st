actions
saveSnapshot: aSnapshot named: aString
	| version |
	version := self versionWithSnapshot: aSnapshot name: aString.
	repository storeVersion: version.
	^ version info
	
actions
addVersionWithSnapshot: aSnapshot name: aString
	| version |
	version := self versionWithSnapshot: aSnapshot name: aString.
	self addVersion: version.
	^ version info
instance creation
fromVersionString: aVersionString
	"Create a new version from a versionString."

	| version |
	version := self new.
	version versionString: aVersionString .
	^ version

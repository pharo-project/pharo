instance creation
fromVersionName: aVersionName
	"Create a new version from a versionName (a file reference)."

	| version names |
	names := MetacelloCommonMCSpecLoader nameComponentsFrom: aVersionName.
	version := self new.
	version name: names first.
	version author: (names at: 2).
	version versionString: (names at: 3).
	^ (version versionString = 0) ifTrue: [ nil ] ifFalse: [ version ]

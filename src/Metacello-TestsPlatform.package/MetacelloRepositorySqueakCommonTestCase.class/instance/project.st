accessing
project
     "force new version as workaround for https://code.google.com/p/smalltalk-hub/issues/detail?id=21"

	| constructor project |
	"Construct Metacello project"
	constructor := MetacelloVersionConstructor on: self.
	project := constructor project.
	project loader: MetacelloNullRecordingMCSpecLoader new.
	^project
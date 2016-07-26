private
loadVersionFromFileNamed: fn
	| version |
	version := self repository loadVersionFromFileNamed: fn.
	^version
		ifNil: [self inboxRepository loadVersionFromFileNamed: fn]
tests
testRepositoryFallback
	| version |
	version := self snapshot.
	self assert: (repositoryGroup versionWithInfo: version info) == version.
	versions removeKey: version info.
	versions2 at: version info put: version.
	self assert: ( repositoryGroup versionWithInfo: version info) == version.
	versions2 removeKey: version info.
	self should: [repositoryGroup versionWithInfo: version info] raise: Error.
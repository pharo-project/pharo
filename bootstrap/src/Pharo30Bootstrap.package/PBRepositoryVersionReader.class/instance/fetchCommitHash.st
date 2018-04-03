accessing
fetchCommitHash
	(OSEnvironment current getEnv: 'TRAVIS_COMMIT') ifNotNil: [ :x | ^ x ].

	^ [ repository headCommit commitId ] ifError: [ 'UNKNOWN' ]
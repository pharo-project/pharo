accessing
commitHash
	^ commitHash ifNil: [ commitHash := self fetchCommitHash ]
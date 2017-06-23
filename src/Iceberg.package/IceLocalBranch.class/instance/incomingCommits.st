commits
incomingCommits
	^ self upstream
		ifNotNil: [ self upstream commitsNotIn: self ]
		ifNil: [ #() ]
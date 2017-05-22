accessing
commitId
	^ commitId ifNil: [ commitId := LGitId new ]
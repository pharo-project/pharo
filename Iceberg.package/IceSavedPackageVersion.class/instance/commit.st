accessing
commit
	^ commit ifNil: [ commit := self repository allCommits detect: [ :aCommit | 
			aCommit commitId = self entry commitID hex ]]
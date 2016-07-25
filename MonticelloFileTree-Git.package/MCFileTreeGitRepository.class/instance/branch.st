accessing-repository
branch
	^ branch ifNil: [ branch := self currentBranch ]
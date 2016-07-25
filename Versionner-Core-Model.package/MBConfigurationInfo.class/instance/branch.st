accessing
branch

	branch ifNil: [ branch := self extractBranchFrom: self workingCopy ].
	^branch
actions
switchBranch
	| branch |
	branch := self pickBranch.
	branch ifNil: [ ^ self ].
	self switchBranchTo: branch
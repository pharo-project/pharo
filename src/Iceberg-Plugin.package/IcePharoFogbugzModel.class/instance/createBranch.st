actions
createBranch
	| branchName |
	
	branchName := '{1}-{2}' format: { 
		issueNumberText text trim. 
		issueText text trim }.
	acceptBlock ifNotNil: [ acceptBlock value: branchName ].
	self window delete
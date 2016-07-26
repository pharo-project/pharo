updating
update: aSymbol
	aSymbol = #issueNumber ifTrue: [ self downloadIssueTitle ].
	self changed: #okEnabled.
	super update: aSymbol
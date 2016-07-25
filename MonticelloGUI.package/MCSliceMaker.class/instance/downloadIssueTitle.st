actions
downloadIssueTitle
	[self info setTitleFromIssue]
		on: Error
		do: [:ex | ^ self informFailedWith: ex].
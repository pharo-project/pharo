issue tracker
setTitleFromIssue
	| sanitizedIssueTitle |
	sanitizedIssueTitle := self sanitizeIssueTitle: self issue title.
	self title: sanitizedIssueTitle
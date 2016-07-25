accessing
title: aString
	title := self sanitizeIssueTitle: aString.
	self changed: #title
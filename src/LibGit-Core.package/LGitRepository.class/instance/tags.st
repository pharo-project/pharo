accessing
tags
	| array strings |
	array := LGitStringArray externalNew.
	self withReturnHandlerDo: [
		self
			tag_list: array
			repo: self ].
	strings := array strings.
	array free.
	^ strings
	
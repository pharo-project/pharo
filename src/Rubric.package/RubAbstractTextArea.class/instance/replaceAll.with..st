find-replace
replaceAll: aRegex with: aText
	self
		handleEdit: [self editor replaceAll: aRegex with: aText].

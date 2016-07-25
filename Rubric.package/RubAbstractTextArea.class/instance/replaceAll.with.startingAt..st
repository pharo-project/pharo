find-replace
replaceAll: aRegex with: aText startingAt: startIdx
	self
		handleEdit: [self editor replaceAll: aRegex with: aText startingAt: startIdx].

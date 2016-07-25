find-replace
findNextString: aSubstring startingAt: searchIdx
	| where |
	self
		handleEdit: [where := self editor findNextString: aSubstring startingAt: searchIdx].
	^ where

tests
testSiblingOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := Path / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
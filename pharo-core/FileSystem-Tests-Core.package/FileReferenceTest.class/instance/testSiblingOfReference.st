tests
testSiblingOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
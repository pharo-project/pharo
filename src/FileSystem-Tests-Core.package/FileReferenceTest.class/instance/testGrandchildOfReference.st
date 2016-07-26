tests
testGrandchildOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'griffle' / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
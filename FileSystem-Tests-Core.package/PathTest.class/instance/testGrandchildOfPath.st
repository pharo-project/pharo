tests
testGrandchildOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := griffle / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
tests
testWaitMaxMilliseconds
	| monitor |
	monitor := Monitor new.
	monitor critical: [ monitor waitMaxMilliseconds: 10 ].
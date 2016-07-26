testing
testnewC

	| alien |
	alien := 'foo' asAlien.
	self should: [alien strcpy = 'foo'].
	alien free. 
testing
teststrcpy

	| alien |
	alien := 'foo' asAlien.
	self should: [alien strcpy = 'foo'].
	self should: [(alien strcpyFrom: 2) = 'oo'].
	alien free.
	alien := 'foo' asAlien.
	self should: [alien strcpyUTF8 = 'foo'].
	self should: [(alien strcpyUTF8From: 2) = 'oo'].
	alien free.

tests
testCategoryMatching
	
	self assert: (self organizer category: 'Compression-Archives' matches: 'Compression').
	self deny: (self organizer category: 'Compression' matches: 'Compression-Archives').
	self assert: (self organizer category: 'Compression' matches: 'Compression').
	self assert: (self organizer category: 'Compression-Arh' matches: 'Compression').
	self deny: (self organizer category: 'XXXX' matches: 'Compression-Archives').
testing
testAccessing
	| alien |
	alien := Alien newCString: 'foo'.
	alien freeAfter: [:cstring | 
			self should: [cstring dataSize = 4].
			self should: [cstring strcpy = 'foo']].
	self should: [alien address = 0].
	self should: [alien dataSize = 0].
combining
+ aCharacter 
	| last |
	last := self sequence last.
	self sequence removeLast.
	self addShortcut: last + aCharacter.
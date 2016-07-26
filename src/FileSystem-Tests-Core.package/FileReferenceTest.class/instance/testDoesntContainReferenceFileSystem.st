tests
testDoesntContainReferenceFileSystem
	| ref other |
	ref := filesystem  * 'griffle'.
	other := FileSystem memory / 'griffle' / 'nurp'.
	self deny: (ref contains: other)
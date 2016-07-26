tests
testDoesntContainReferencePath
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem * 'nurp'.
	self deny: (ref contains: other)
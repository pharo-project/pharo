tests
testDoesntContainPath
	| ref |
	ref := filesystem * 'griffle'.
	self deny: (ref contains: (Path * 'nurp'))
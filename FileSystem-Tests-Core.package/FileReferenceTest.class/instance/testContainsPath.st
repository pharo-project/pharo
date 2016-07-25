tests
testContainsPath
	| ref |
	ref := filesystem  * 'griffle'.
	self assert: (ref contains: (ref / 'nurp') path)
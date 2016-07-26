accessing
includesSubstring: substring
	^ substring isEmpty or: [ (self findString: substring startingAt: 1) > 0 ]
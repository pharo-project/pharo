utilities
string: aString includesSubstring: aSubstring

	^ (aString findString: aSubstring startingAt: 1) > 0
accessing
findWordStart: key startingAt: start
	| ind |
	"HyperCard style searching.  Answer the index in self of the substring key, when that key is preceeded by a separator character.  Must occur at or beyond start.  The match is case-insensitive.  If no match is found, zero will be returned."

	ind := start.
	[ind := self findString: key startingAt: ind caseSensitive: false.
	ind = 0 ifTrue: [^ 0].	"not found"
	ind = 1 ifTrue: [^ 1].	"First char is the start of a word"
	(self at: ind-1) isSeparator] whileFalse: [ind := ind + 1].
	^ ind	"is a word start"
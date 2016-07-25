selecting
selectLastMatch: aString
	"Answer the last subitem that has text that includes the given substring.
	Answer nil if none.
	Disable non-matching items and enable matching items."

	|lastMatch|
	self items reverseDo: [:item | | match |
		match := aString isEmpty or: [item contents asString asLowercase includesSubstring: aString].
		item isEnabled: match.
		(match and: [lastMatch isNil]) ifTrue: [lastMatch := item]].
	^lastMatch
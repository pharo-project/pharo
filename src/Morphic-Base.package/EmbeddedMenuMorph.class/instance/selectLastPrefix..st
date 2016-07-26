selecting
selectLastPrefix: aString
	"Answer the last subitem that has text that matches the given prefix.
	Answer nil if none.
	Disable non-matching items and enable matching items."

	|lastMatch|
	self items reverseDo: [:item | | match |
		match := aString isEmpty or: [item contents asString asLowercase beginsWith: aString].
		item isEnabled: match.
		(match and: [lastMatch isNil]) ifTrue: [lastMatch := item]].
	^lastMatch
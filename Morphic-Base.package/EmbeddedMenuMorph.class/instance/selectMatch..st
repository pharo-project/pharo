selecting
selectMatch: aString
	"Answer the first subitem that has text that includes the given substring.
	Answer nil if none.
	Disable non-matching items and enable matching items."

	|firstMatch|
	self items do: [:item | | match |
		match := aString isEmpty or: [item contents asString asLowercase includesSubstring: aString].
		item isEnabled: match.
		(match and: [firstMatch isNil]) ifTrue: [firstMatch := item]].
	^firstMatch
selecting
selectPrefix: aString
	"Answer the first subitem that has text that matches the given prefix.
	Answer nil if none.
	Disable non-matching items and enable matching items."

	| firstMatch|
	self items do: [:item | | match |
		match := aString isEmpty or: [item contents asString asLowercase beginsWith: aString].
		item isEnabled: match.
		(match and: [firstMatch isNil]) ifTrue: [firstMatch := item]].
	^firstMatch
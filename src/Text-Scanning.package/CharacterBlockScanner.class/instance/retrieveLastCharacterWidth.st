private
retrieveLastCharacterWidth
	| lastCharacter |
	lastIndex > text size ifTrue: [^lastCharacterWidth := 0].
	specialWidth ifNotNil: [^lastCharacterWidth := specialWidth].
	lastCharacter := text at: lastIndex.
	(lastCharacter charCode >= 256 or: [(stopConditions at: lastCharacter charCode + 1) isNil])
		ifTrue: [lastCharacterWidth := font widthOf: (text at: lastIndex)].
	"if last character was a stop condition, then the width is already set"
	^lastCharacterWidth
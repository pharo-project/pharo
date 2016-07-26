menu messages
setSelectorSearch: aStringOrText 
	"Make the current selection as a selector search in current FindReplaceService."
	| regex |
	regex := aStringOrText asString trimBoth.
	regex := regex copyReplaceAll: '#' with: ''.
	regex := regex copyReplaceAll: ' ' with: ''.
	self findText: regex isRegex: false caseSensitive: true entireWordsOnly: false.
	^ true
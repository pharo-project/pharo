menu messages
setSearch: aString
	"Make the current selection, if any, be the current search string."
	self findText: aString isRegex: false.
	^ true
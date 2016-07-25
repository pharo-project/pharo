menu messages
setSearchString
	"Make the current selection, if any, be the current search string."
	self closeTypeIn.
	self setSearch: self selection string.
	^ true
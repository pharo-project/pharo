accessing
filter: aString
	"Set the value of filter, used to match the valid choices."

	filter := aString.
	self changed: #filter.
	self filterMorph ifNil: [^self].
	(self choiceMenus ifNil: [^self]) do: [:embeddedMenu |
		embeddedMenu selectItem: nil event: nil]. "clear selection in other menus"
	self choiceMenus do: [:embeddedMenu | 
		embeddedMenu selectMatch: self filter asLowercase].
	self filterMorph hasKeyboardFocus ifFalse: [
		self activeHand newKeyboardFocus: self filterMorph.
		self filterMorph selectFrom: filter size + 1 to: filter size + 1]
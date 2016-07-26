accessing
filter: aString
	"Set the value of filter used to match the choices.
	Clear any selected items here so that potential requests for
	non-listed items can be handled."
	
	super filter: aString.
	(self choiceMenus ifNil: [^self]) do: [:embeddedMenu |
		embeddedMenu selectItem: nil event: nil]. "clear selection in other menus"

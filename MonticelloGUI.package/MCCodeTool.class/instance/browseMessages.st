menus
browseMessages
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all implementors of the selector chosen."

	self systemNavigation browseAllImplementorsOf: (self selectedMessageName ifNil: [ ^nil ])
menu & halo
addToggleItemsToHaloMenu: aCustomMenu 
	"Add toggle-items to the halo menu"
	
	super addToggleItemsToHaloMenu: aCustomMenu.
	aCustomMenu
		addUpdating: #enabledString
		target: self
		selector: #toggleEnabled
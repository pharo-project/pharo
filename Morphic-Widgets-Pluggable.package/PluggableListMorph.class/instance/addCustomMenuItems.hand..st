menus
addCustomMenuItems:  aMenu hand: aHandMorph
	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'list font...' target: self selector: #setListFont.
	aMenu add: 'copy list to clipboard' target: self selector: #copyListToClipboard.
	aMenu add: 'copy selection to clipboard' target: self selector: #copySelectionToClipboard
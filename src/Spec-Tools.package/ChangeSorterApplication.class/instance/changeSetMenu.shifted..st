menu
changeSetMenu: aMenu shifted: isShifted
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	aMenu addTitle: 'Change Set'.
	self changeSetMenu1: aMenu shifted: isShifted.
	self changeSetMenu2: aMenu shifted: isShifted.
	^ aMenu
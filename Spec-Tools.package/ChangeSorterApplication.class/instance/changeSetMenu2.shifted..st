menu
changeSetMenu2: aMenu shifted: isShifted 
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	aMenu addAllFromPragma: 'changeSorterChangeSetList2Menu' target: self.
	^aMenu

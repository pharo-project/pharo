menu
changeSetMenu1: aMenu shifted: isShifted 
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"
	
	aMenu addAllFromPragma: 'changeSorterChangeSetList1Menu' target: self.

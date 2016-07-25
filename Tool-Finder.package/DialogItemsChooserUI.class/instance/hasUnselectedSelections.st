unselectedList
hasUnselectedSelections
	"Answer whether the unselected list has selected items."

	^unselectedSelectionList anySatisfy: [:selected | selected]
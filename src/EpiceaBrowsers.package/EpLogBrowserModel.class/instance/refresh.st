refreshing
refresh

	"Refresh log"
	itemsModel resetSelection. "Needed, else #items: can silently fail"
	itemsModel items: self filteredEntryReferences.

	"Refresh toolbar"	
	toolbarModel refresh.

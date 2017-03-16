refreshing
refresh

	"Refresh filters"
	filtersModel roots: andFilter subFilters.
	filtersTabModel label: self activeFiltersLabel.

	"Refresh log"
	itemsModel resetSelection. "Needed, else #items: can silently fail"
	itemsModel items: self filteredEntryReferences.
initialization
initialize

	super initialize.

	classesList := nil.
	classesListElements := nil.
	classesSelection := Dictionary new.
	cachedHierarchy := SortHierarchically new.
	showHierarchy := false.
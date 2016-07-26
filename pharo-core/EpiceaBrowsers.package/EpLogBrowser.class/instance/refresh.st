refreshing
refresh

	filtersModel roots: andFilter subFilters.
	view refresh.
	itemsModel roots: view rootItems.
	statusBarModel label: self log descriptionString.
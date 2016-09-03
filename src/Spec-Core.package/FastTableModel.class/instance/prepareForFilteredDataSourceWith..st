private
prepareForFilteredDataSourceWith: items

	listHolder := items asValueHolder.
	listHolder whenChangedDo: [ self refreshListItems ].
	listItemsCache := nil
api
items: aCollection

	associationsWrapperToItem removeAll.
	pickedItemsHolder removeAll.
	listModel items: (aCollection collectWithIndex: [:e :i | self wrap: e at: i ]).
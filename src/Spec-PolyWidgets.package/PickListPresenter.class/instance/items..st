api
items: aCollection

	associationsWrapperToItem removeAll.
	pickedItemsHolder removeAll.
	listPresenter items: (aCollection collectWithIndex: [:e :i | self wrap: e at: i ]).
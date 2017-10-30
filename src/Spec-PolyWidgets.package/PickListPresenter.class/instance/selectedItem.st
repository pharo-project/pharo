api
selectedItem

	^ associationsWrapperToItem 
		at: listPresenter selectedItem
		ifAbsent: [ nil ]
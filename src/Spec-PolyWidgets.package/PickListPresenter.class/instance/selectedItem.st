api
selectedItem

	^ associationsWrapperToItem 
		at: listModel selectedItem
		ifAbsent: [ nil ]
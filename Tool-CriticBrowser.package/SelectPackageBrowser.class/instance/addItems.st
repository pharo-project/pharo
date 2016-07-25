private
addItems
	toSelectModel selectedItems
		ifNotNil: [ :items |  
			selectedModel items: (self sort: selectedModel listItems withAll: items).
			toSelectModel items: (self sort: toSelectModel listItems withoutAll: items) ].
	toSelectModel updateList.
	selectedModel updateList.  	  
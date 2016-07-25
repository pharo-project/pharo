private
removeItems
	selectedModel selectedItems
		ifNotNil: [:items | 
			toSelectModel items: (self sort: toSelectModel listItems withAll: items).
			selectedModel items: (self sort: selectedModel listItems withoutAll: items) ].
	toSelectModel updateList.
	selectedModel updateList.  	  
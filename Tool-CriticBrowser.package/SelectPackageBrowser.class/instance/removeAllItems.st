private
removeAllItems
	| tmp |
	tmp := self 
		sort: toSelectModel listItems
		withAll: selectedModel listItems.
	toSelectModel items: tmp.
	selectedModel items: #().
	toSelectModel updateList.
	selectedModel updateList.  	  
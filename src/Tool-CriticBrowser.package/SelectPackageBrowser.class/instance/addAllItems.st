private
addAllItems
	| tmp |
	tmp := self 
		sort: selectedModel listItems
		withAll: toSelectModel listItems.
	selectedModel items: tmp.
	toSelectModel items: #().
	toSelectModel updateList.
	selectedModel updateList.  	  
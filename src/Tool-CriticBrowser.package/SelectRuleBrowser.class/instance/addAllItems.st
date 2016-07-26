api
addAllItems
	
	toSelectModel roots do: [ :item | selectedRules addAll: item leaves].
	self updateTree.  	  
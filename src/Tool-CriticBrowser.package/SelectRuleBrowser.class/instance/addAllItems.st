api
addAllItems
	
	toSelectModel roots do: [ :group | selectedRules addAll: group rules].
	self updateTree.  	  
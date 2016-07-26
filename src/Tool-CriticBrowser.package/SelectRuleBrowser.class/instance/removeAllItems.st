api
removeAllItems
	
	selectedModel roots do: [ :item |  
		selectedRules removeAllFoundIn: item leaves].
	self updateTree 	  
api
removeAllItems
	
	selectedModel roots do: [ :item |  
		selectedRules removeAllFoundIn: item value ].
	self updateTree 	  
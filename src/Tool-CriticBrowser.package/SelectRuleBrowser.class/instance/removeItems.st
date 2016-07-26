api
removeItems
	
	selectedModel selectedItem
		ifNotNil: [ :item | selectedRules removeAllFoundIn: item content leaves ].
	self updateTree  	  
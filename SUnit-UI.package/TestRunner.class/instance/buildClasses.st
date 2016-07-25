building
buildClasses

	^ PluggableListMorph new
		on: self
		list: #classList 
		primarySelection: #classSelected 
		changePrimarySelection: #classSelected: 
		listSelection: #classAt: 
		changeListSelection: #classAt:put: 
		menu: #classMenu:  
		keystroke: nil;
		yourself
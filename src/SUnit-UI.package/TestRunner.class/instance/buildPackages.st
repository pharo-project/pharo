building
buildPackages

	^ PluggableListMorph new
		on: self
		list: #packageList 
		primarySelection: #packageSelected 
		changePrimarySelection: #packageSelected: 
		listSelection: #packageAt: 
		changeListSelection: #packageAt:put: 
		menu: #packageMenu:  
		keystroke: nil;
		yourself
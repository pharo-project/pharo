private-actions
deleteSelectedTabs
	toolbar selectedTab 
		ifNotNil: [ toolbar deleteSelectedTabs ] 
		ifNil: [ self owner takeKeyboardFocus ]
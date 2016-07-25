menu
mainInspectSubMenu: aMenu 
	aMenu 
		add: 'Inspect (i)' 
		target: self
		selector: #inspectSelectedObjectInNewWindow.
				
	aMenu
		add: 'Explore (I)' 
		target: self
		selector: #exploreSelectedObject.
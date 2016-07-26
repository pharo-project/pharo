accessing
menu: menu shifted: b 
	menu
		add: 'Expand all' 
		target: self
		selector: #expandAll.
	menu
		add: 'Browse (b)' 
		target: self
		selector: #browseItem.
	menu
		add: 'Inspect (i)' 
		target: self
		selector: #inspectItem.
	menu
		add: 'Explore (I)' 
		target: self
		selector: #exploreItem.

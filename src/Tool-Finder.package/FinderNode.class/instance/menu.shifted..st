events handling
menu: menu shifted: aBoolean
	menu
		add: 'Browse (b)'
		target: self
		selector: #browse.
	self addInspectMenuItem: menu.
		
	menu addLine.
	menu
		add: 'Expand All'
		target: self model
		selector: #expandAll.
	menu
		add: 'Collapse All'
		target: self model
		selector: #collapseAll.
	^menu
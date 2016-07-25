event handling
menu: menu shifted: aBoolean
	menu
		add: 'Hierarchy (h)'
		target: self
		selector: #browseHierarchy.
	menu
		add: 'References (N)'
		target: self
		selector: #browseReferences.
	^ super menu: menu shifted: aBoolean.
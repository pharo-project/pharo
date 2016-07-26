event handling
menu: menu shifted: aBoolean
	menu
		add: 'References (N)'
		target: self
		selector: #browseReferences.
	^ super menu: menu shifted: aBoolean.
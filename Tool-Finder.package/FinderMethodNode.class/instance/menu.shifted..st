events handling
menu: menu shifted: aBoolean
	
	menu
		add: 'Implementors (m)'
		target: self
		selector: #browseImplementors.
		
	menu
		add: 'Senders (n)'
		target: self
		selector: #browseSenders.
		
	^ super menu: menu shifted: aBoolean.
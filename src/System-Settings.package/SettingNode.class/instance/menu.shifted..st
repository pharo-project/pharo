menu
menu: menu shifted: b 
	menu
		add: 'Browse (b)'
		target: self
		selector: #browseDeclaration.		
	self item isExportable ifFalse: [ ^self ].
	(menu
		add: 'Save to disk'
		target: self
		selector: #saveToDisk)
			enablementSelector: #isNotCurrentValueStored.
	(menu
		add: 'Load from disk'
		target: self
		selector: #loadFromDisk)
			enablementSelector: #isAnotherValueStored.
	(menu
		add: 'Delete stored value'
		target: self
		selector: #deleteStoredValue)
			enablementSelector: #isStored.
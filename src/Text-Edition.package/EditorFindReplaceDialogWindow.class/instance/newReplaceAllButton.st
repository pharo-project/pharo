user-interface
newReplaceAllButton
	^ (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #replaceAll
		arguments: nil
		getEnabled: #replaceAllEnabled
		label: 'Replace all' translated
		help: 'Replace all occurences' translated) hResizing: #rigid;
		 vResizing: #rigid
user interface
toolBarIn: aMorph 
	| toolBar |
	toolBar := aMorph newToolbar: {
	aMorph
		newButtonFor: self
		getState: nil
		action: #expandAll
		arguments: nil
		getEnabled: nil
		label: 'Expand all' translated
		help: 'Expand all tree nodes' translated.
	aMorph newToolSpacer.
	aMorph newToolSpacer.
	aMorph newLabel: 'Search for: ' translated.
	(SearchMorph
				on: self
				list: #searchedTextList
				selected: #searchedText
				changeSelected: #changeSearchedText:
				useIndex: false 
				addToList: #addToSearchedTextList:
				class: String
				getEnabled: nil) hResizing: #spaceFill; 
				ghostText: 'Hit return to accept' translated.
	aMorph
			newCheckboxFor: self
			getSelected: #regexpSearch
			setSelected: #regexpSearch:
			getEnabled: nil
			label: 'Regexp' 
			help: 'Filter according to a regular expression' translated.
	aMorph newToolSpacer.	
	aMorph newToolSpacer.
	(aMorph
				newButtonFor: self
				getState: nil
				action: #choosePackagesIn:
				arguments: {aMorph}
				getEnabled: nil
				label: 'Choose packages' translated 
				help: 'Per default, SettingBrowser shows all the setting of the system. You can restrict what is displayed to some packages that contains setting defintions.' translated).
	(aMorph
				newButtonFor: self
				getState: nil
				action: #storeSettings
				arguments: nil
				getEnabled: nil
				label: 'Store Settings' translated 
				help: 'Store the current settings' translated).
	(aMorph
				newButtonFor: self
				getState: nil
				action: #loadSettings
				arguments: nil
				getEnabled: nil
				label: 'Load Settings' translated 
				help: 'Load the current settings from the disk' translated).
	aMorph newToolSpacer}.
	^ toolBar
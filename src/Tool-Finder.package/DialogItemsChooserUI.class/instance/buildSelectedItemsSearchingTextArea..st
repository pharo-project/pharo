build items
buildSelectedItemsSearchingTextArea: aWindow

	^aWindow
		newAutoAcceptTextEntryFor: self
		get: #selectedItemsSearchingString
		set: #selectedItemsSearchingString:
		class: String
		getEnabled: nil
		help: 'Enter the name of a package' translated
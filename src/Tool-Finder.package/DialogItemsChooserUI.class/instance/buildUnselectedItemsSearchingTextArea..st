build items
buildUnselectedItemsSearchingTextArea: aWindow

	^aWindow
		newAutoAcceptTextEntryFor: self
		get: #unselectedItemsSearchingString
		set: #unselectedItemsSearchingString:
		class: String
		getEnabled: nil
		help: 'Enter the name of a package' translated
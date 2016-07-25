focus handling
giveFocusToList
	list ifEmpty: [ ^ self giveFocusToSearch].
	
	self listIndex: (( listIndex max: 1 ) min: list size).
	listMorph takeKeyboardFocus.
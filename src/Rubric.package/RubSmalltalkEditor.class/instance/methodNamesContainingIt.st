menu messages
methodNamesContainingIt
	"Open a browser on methods names containing the selected string"

	self lineSelectAndEmptyCheck: [ ^ self ].
	Cursor wait
		showWhile: [ self model interactionModel systemNavigation
				browseMethodsWhoseNamesContain: self selection string trimBoth ]
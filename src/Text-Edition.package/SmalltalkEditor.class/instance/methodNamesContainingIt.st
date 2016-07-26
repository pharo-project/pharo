menu messages
methodNamesContainingIt
	"Open a browser on methods names containing the selected string"

	self lineSelectAndEmptyCheck: [ ^ self ].
	Cursor wait showWhile: [ 
		model systemNavigation  browseMethodsWhoseNamesContain: self selection string trimBoth ]
	
menu messages
methodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).   EXTREMELY slow!"

	self lineSelectAndEmptyCheck: [ ^ self ].
	self model interactionModel systemNavigation browseMethodsWithSourceString: self selection string matchCase: false
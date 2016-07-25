accessing
emphasisHere: aListOfTextAttribute 
	emphasisHere := aListOfTextAttribute
				ifNotNil: [:l | l reject: [:a | TextSelectionColor = a class]]
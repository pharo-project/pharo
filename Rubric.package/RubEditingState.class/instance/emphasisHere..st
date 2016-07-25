accessing
emphasisHere: aListOfTextAttribute
	emphasisHere := aListOfTextAttribute ifNotNil: [ :l | l reject: [ :a | RubTextSelectionColor = a class ] ]
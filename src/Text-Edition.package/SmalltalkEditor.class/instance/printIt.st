do-its
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	| printString |
	self
		evaluateSelectionAndDo: [ :result | 
			printString := [ result printString ]
				on: Error
				do: [ '<error in printString: try ''Inspect it'' to debug>' ].
			self afterSelectionInsertAndSelect: printString ]
private
updateTextArea

	| textArea |
	
	textArea := self paragraph textArea.
	
	textArea recomputeSelection.
	textArea cursor updateBounds.
	
	(textArea scrollPane ifNil: [ textArea ]) changed
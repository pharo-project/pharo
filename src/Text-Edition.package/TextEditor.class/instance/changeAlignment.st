menu messages
changeAlignment
	"Interactively change the alignment of the text currently being edited."
	| reply  |
	reply := UIManager default 
				chooseFrom:  self alignmentChoices 
				values:  self alignmentChoices.	
	reply ifNil: [^self].
	self applyAttribute: (TextAlignment perform: reply).
	^ true
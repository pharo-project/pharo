editing
stylerStyled: styledCopyOfText
	next ifNil: [ ^ self ].
	(text notNil and:[text string = styledCopyOfText string])
		ifFalse: [ ^ self ].
	text runs: styledCopyOfText runs.
	"must take care to recompose. If not, the textArea 
	markIndex and pointIndex may be inconsistent"
	self paragraph 
		recomposeFrom: replaceStart 
		to: replaceStart + text size - 1 
		delta: text size - (replaceStop - replaceStart + 1).	
			
	"The selection and then the cursor bounds has to be updated 
	to take possible text attribute changes into account"
	
	self updateTextArea
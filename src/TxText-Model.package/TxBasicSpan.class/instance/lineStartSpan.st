accessing
lineStartSpan
	" return the closest line start span "
	self reverseDo: [ :span | 
		span isAtLineStart ifTrue: [ ^ span ]].
	Error signal: 'Should not happen'.
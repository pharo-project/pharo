do-its
highlightEvaluateAndDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	
	self highlightAndEmptyCheck: [ ^ '' ].
	^ self 
		evaluate: self highlightedTextAsStream
		andDo: [:result | aBlock value: result]
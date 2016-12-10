correcting
variable: varNode shadows: semVar
	compilationContext interactive
		ifTrue: [ 
			OCSemanticError new
				node: node;
				compilationContext: compilationContext;
				messageText: self stringMessage;
				signal ]
		ifFalse: [ self showWarningOnTranscript ].

examples
selectionEvaluationPopper
	"
	self selectionEvaluationPopper
	"
	GLMPrintPopper new 
		evaluateAndOpenFromRubric: (RubScrolledTextMorph new 
			beForSmalltalkCode;
			updateTextWith: '1+2+4';
			setSelection: (1to:6))
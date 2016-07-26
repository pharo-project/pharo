events-alarms
popupPrint
	"Evaluates selected code and prints it in a popper"
	textMorph textArea editor highlightEvaluateAndDo: [ :result | 
		textMorph shoutStyler style: textMorph text.
		GLMPrintPopper new 
			openFromRubric: textMorph textArea withResult: result ]
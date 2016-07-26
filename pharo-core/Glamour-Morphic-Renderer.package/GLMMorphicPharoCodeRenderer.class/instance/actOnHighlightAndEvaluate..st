callbacks
actOnHighlightAndEvaluate: ann 

	"We get here as a result of an announcement.
	If we do not defer the execution and an exception is triggered by the execution of the code
	then resuming the execution in the debugger will trigger an error when unwinding the stack."
	UIManager default defer: [ 
		textMorph textArea handleEdit: [
			textMorph textArea editor highlightEvaluateAndDo: ann action .
			textMorph shoutStyler style: textMorph text. ] ]
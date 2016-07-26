callbacks
actOnEvaluateSelection: ann 

	"We get here as a result of an announcement.
	If we do not defer the execution and an exception is triggered by the execution of the code
	then resuming the execution in the debugger will trigger an error when unwinding the stack."
	ann isSyncronous ifTrue: [ 
		textMorph textArea editor evaluateSelectionAndDo: ann action.
		^ self ]. 
	UIManager default defer: [ 
		textMorph textArea editor evaluateSelectionAndDo: ann action ]
	
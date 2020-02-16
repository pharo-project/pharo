"
replaceTokenInEditorWith: aString

	""Main API with the completion context.
	Replace the current completion token where the caret is by aString.
	aString may contain a keyword selector string where each keyword is separated by two spaces (between:  and:)
	After replacing, set the caret after the first keyword.
	The completion context uses this API to insert text into the text editor""
	
	self subclassResponsibility
"
Class {
	#name : #AbstractCompletionEngine,
	#superclass : #Object,
	#category : #'NECompletion-Model'
}

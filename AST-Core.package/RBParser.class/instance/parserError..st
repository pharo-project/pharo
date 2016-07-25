error handling
parserError: aString 
	"Let the errorBlock try to recover from the error."
	| errorNode errorMessage errorPosition newSource |
	errorNode := self errorBlock cull: aString cull: self errorPosition cull: self.
	errorNode ifNotNil: [ ^ errorNode ].
	currentToken isError 
		ifTrue: [ errorMessage := currentToken cause. errorPosition := currentToken location ] 
		ifFalse: [errorMessage := aString. errorPosition := currentToken start].
	
	newSource := SyntaxErrorNotification
						inClass: Object
						category: nil
						withCode: source
						doitFlag: false
						errorMessage: errorMessage
						location: errorPosition.
	
	"If the syntax error notification is resumed, then the source was corrected and we have to announce that parsing can restart."					
	ReparseAfterSourceEditing signalWithNewSource: newSource
Extension { #name : #OpalCompiler }

{ #category : #'*GT-SpotterExtensions-Core' }
OpalCompiler >> silentlyDo: aBlock1 exceptionDo: aBlock2 [
	aBlock1
		on: Error, SyntaxErrorNotification, OCSemanticWarning, OCSemanticError
		do: aBlock2
]

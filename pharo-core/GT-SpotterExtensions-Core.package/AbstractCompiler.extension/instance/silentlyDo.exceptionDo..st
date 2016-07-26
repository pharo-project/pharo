*GT-SpotterExtensions-Core
silentlyDo: aBlock1 exceptionDo: aBlock2
	aBlock1
		on: Error, SyntaxErrorNotification, OCSemanticWarning, OCSemanticError
		do: aBlock2
testing
testIsMessageSentInSystemAnswersMethodReference
	| classesSendingMessage sentMessageSelector anyOne |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [self classFactory newClassInCategory: #One].
	5 timesRepeat: [self classFactory newClassInCategory: #Two].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2), (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [:class|	
		class compileSilently: 'meth self ', sentMessageSelector].
	anyOne :=(self systemNavigationToTest allSendersOf: sentMessageSelector) anyOne.

	self assert: (anyOne isKindOf: RGMethodDefinition) 
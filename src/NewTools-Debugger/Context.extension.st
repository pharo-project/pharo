Extension { #name : #Context }

{ #category : #'*NewTools-Debugger' }
Context >> findFailedAssertionContextInSenderChain [

	^ (self findContextSuchThat: [ :ctx | 
		   (ctx hasTemporaryVariableNamed: #actual) and: [ 
			   ctx hasTemporaryVariableNamed: #expected ] ]) ifNil: [ 
		  StDebuggeAssertionFailureContextNotFound new signal ]
]

{ #category : #'*NewTools-Debugger' }
Context >> findFailedAssertionResultsInSenderChain [
	|assertionContext assertionResult|
	assertionContext := self findFailedAssertionContextInSenderChain.
	assertionResult := StDebuggerAssertionFailureResult new.
	assertionResult actual: (assertionContext readVariableNamed: #actual).
	assertionResult expected: (assertionContext readVariableNamed: #expected).
	^assertionResult
]

Extension { #name : #DebugSession }

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> browse: aContext [

	aContext method browse
]

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> isInterruptedContextAnAssertionFailure [
	| variable exceptionClass |
	variable := (self interruptedContext temporaryVariableNamed:
		             #exception) ifNil: [ ^ false ].
	exceptionClass := variable readInContext: self interruptedContext.
	self flag:
		'Will be simplified when debug sessions will know their exception'.
	^ exceptionClass notNil and: [ 
		  (exceptionClass isKindOf: TestFailure) or: [ 
			  exceptionClass isClass and: [ 
				  exceptionClass includesBehavior: TestFailure ] ] ]
]

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> shortStack [
	^self stackOfSize: self shortStackSize
]

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> shortStackSize [
	^ 20
]

Extension { #name : #DebugSession }

{ #category : #'*Deprecated80' }
DebugSession >> returnValueFrom: aContext [
	"Force a return of a given value to the previous context!"

	| previous expression value |
	self deprecated: 'This method is unused in Pharo and will be removed in Pharo 9.' on: '27-10-2019' in: #Pharo8.
	(self isContextPostMortem: aContext) ifTrue: [^ self].
	
	expression := UIManager default request: 'Enter expression for return value:'.
	value := self class compiler
					source: expression;
			context: aContext;
			receiver: aContext receiver;
			evaluate.
				
	previous := aContext sender.
	"self resetContext: previous."
	interruptedProcess popTo: previous value: value.
	self updateContextTo: previous.
	self contextChanged 
]

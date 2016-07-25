changing suspended state
popTo: aContext value: aValue
	"Replace the suspendedContext with aContext, releasing all contexts 
	 between the currently suspendedContext and it."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: aValue]
				onBehalfOf: self]
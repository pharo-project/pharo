debugging actions
stepOver: aContext
	"Send the selected message in selectedContext, and regain control 
	after the invoked method returns."
	
	| newContext |
	
	(self isContextPostMortem: aContext) ifTrue: [^ self].
	
	newContext := interruptedProcess completeStep: aContext.
	self updateContextTo:  
		(newContext == aContext
			ifTrue: [ (self stepToFirstInterestingBytecodeIn: interruptedProcess) ]
			ifFalse: [ newContext ]).

	self triggerEvent: #stepOver 
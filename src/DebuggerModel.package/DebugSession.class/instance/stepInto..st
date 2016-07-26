debugging actions
stepInto: aContext
	"Send the selected message in selectedContext, and take control in 
	the method invoked to allow further step or send."

	(self isContextPostMortem: aContext) ifTrue: [^ self].

	interruptedProcess step: aContext.
	self updateContextTo: (self stepToFirstInterestingBytecodeIn: interruptedProcess).
	
	self triggerEvent: #stepInto
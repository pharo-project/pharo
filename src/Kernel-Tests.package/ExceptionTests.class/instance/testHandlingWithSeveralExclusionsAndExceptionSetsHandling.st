tests - handling
testHandlingWithSeveralExclusionsAndExceptionSetsHandling

	| wasHandled result |

	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - MessageNotUnderstood - Warning
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil.
		
	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - (MessageNotUnderstood , Warning)
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil.	
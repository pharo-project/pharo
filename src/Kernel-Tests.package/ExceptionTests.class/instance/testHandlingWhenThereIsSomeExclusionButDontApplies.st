tests - handling
testHandlingWhenThereIsSomeExclusionButDontApplies

	| wasHandled result |

	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - MessageNotUnderstood
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil
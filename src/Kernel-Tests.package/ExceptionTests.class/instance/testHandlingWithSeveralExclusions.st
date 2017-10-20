tests - handling
testHandlingWithSeveralExclusions
	| wasHandled |
	wasHandled := false.
	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error - Warning - ZeroDivide
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	self deny: wasHandled.
	
	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error - (Warning , ZeroDivide)
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	self deny: wasHandled.
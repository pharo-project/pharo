*GT-Debugger
spanNewSessionFrom: anotherSession

	^ self sessionClass 
		named: anotherSession name 
		on: anotherSession interruptedProcess 
		startedAt: anotherSession interruptedContext
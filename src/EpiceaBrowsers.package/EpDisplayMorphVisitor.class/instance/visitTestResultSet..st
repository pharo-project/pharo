visitor
visitTestResultSet: anEvent

	^ anEvent results size = 1
		ifTrue: [ 
			self displaySingleTestResult: anEvent ]
		ifFalse: [ 
			self displayMultipleTestResults: anEvent ]
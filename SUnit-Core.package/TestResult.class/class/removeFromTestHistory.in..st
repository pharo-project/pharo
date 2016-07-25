history
removeFromTestHistory: aSelector in: aTestCaseClass
	| lastRun |
	
	lastRun := self historyFor: aTestCaseClass.
	#(#passed #failures #errors) do:
		[ :set | (lastRun at: set) remove: aSelector ifAbsent: []].

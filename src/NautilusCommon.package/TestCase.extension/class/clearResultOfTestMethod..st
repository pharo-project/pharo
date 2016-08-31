*NautilusCommon
clearResultOfTestMethod: aMethod
 	#(passed failures errors) do: [ :each |
		(self lastRun at: each)
			remove: aMethod selector
			ifAbsent: [  ] ]
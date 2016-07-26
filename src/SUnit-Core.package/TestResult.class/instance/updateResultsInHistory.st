history
updateResultsInHistory
	|classesToNotify|
	classesToNotify:= Set new.
	#(#passed #failures #errors) do: [ :status | 
		(self perform: status) do: [ :testCase | 
			classesToNotify add:testCase class.
			self class updateTestHistoryFor: testCase status: status ] ].
	classesToNotify do:[:cl | 
		cl historyAnnouncer announce: (TestSuiteEnded result: cl)]
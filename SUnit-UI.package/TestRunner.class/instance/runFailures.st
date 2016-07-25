actions
runFailures
	self result instVarNamed: 'failures' put: Set new.
	self runTestSuites: self failureTestSuites.
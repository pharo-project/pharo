actions
runErrors
	self result instVarNamed: 'errors' put: OrderedCollection new.
	self runTestSuites: self errorTestSuites.
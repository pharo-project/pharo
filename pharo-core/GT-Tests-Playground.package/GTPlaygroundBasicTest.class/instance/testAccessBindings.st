running
testAccessBindings
	|obtainedBindings expectedBindings|

	expectedBindings := Dictionary new.  
	expectedBindings add: (WorkspaceVariable key: #a value: 1).
	expectedBindings add: (WorkspaceVariable key: #b value: 2).
		
	window := playground openOn: (GTPlayPage new saveContent: 'a:=1. b:=a+1').
	playground inspectorPresentation firstPresentation selectLine.
	playground inspectorPresentation firstPresentation evaluateSelection.
		
	obtainedBindings := playground currentBindings.
		
	self assert: obtainedBindings equals: expectedBindings.
	
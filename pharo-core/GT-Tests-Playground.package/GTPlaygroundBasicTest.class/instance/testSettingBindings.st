running
testSettingBindings
	|obtainedResult|
	obtainedResult:=nil.
	window := playground openOn: (GTPlayPage new saveContent: 'number+1').
	playground 
		setBindings: { #number -> 2 } asDictionary.
	playground inspectorPresentation firstPresentation selectLine.
	
	playground inspectorPresentation firstPresentation announce: (GLMEvaluateSelection new 
		action: [ :aResult | obtainedResult := aResult ];
		makeSyncronous).
		
	self assert: obtainedResult equals: 3
	
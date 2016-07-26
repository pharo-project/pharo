public
onExamples: aCollection builder: aBuilder
	(aBuilder item: #'Inspect Examples')
		order: 90003;
		action: [ (self examples: aCollection) inspect ].
	
	(aBuilder item: #'Inspect Return-Values')
		order: 90004;
		action: [ (self examples: aCollection do: #returnValue) inspect ].
		
	(aBuilder item: #'Run Examples')
		order: 10004;
		action: [ (self examples: aCollection do: #run) inspect ].
		
	(aBuilder item: #'Debug Examples')
		order: 10005;
		action: [ (self examples: aCollection do: #debug) ].
		
		
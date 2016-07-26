actions
evaluateSelection

	self announce: (GLMEvaluateSelection new 
		action: [ :result | result ];
		makeSyncronous)
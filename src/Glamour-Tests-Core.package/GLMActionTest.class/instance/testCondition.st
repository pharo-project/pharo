tests
testCondition
	self 
		assert: (GLMAction new isAvailableOn: GLMPresentation new).
	self 
		deny: (GLMAction new 
			condition: [ false ];
			isAvailableOn: GLMPresentation new).
	self 
		deny: (GLMAction new 
			condition: [ :presentation :entity | false ];
			isAvailableOn: GLMPresentation new)
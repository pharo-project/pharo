tests
newParser 
	^ FFIFunctionParser new
		requestor: (FFICallout new
			requestor: self;
			yourself);
		yourself
tests
testJsonMode
	| object |
	object := STON listClass withAll: {
		Float pi.
		'Hello World'.
		true.
		nil.
		STON listClass withAll: #( 1 2 3) asByteArray.
		STON mapClass new 
			at: 'x' put: 1; 
			at: 'y' put: 2; 
			yourself 
	}.
	self serializeAndMaterializeJsonMode: object

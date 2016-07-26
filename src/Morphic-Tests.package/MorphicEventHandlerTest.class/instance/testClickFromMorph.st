tests
testClickFromMorph

	morph eventHandler on: #click send: #value to: true.
	
	self assert: ((morph click: nil) == true)
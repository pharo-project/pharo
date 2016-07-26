tests
testUnknownClasses
	| input object |
	input := 'FooBar { #foo : 1, #bar : true }'.
	self should: [ self materialize: input ] raise: STONReaderError.
	object := STON reader 
		acceptUnknownClasses: true; 
		on: input readStream; 
		next.
	self assert: object class equals: STON mapClass.
	self assert: (object at: #foo) equals: 1.
	self assert: (object at: #bar).
	self assert: (object at: STON classNameKey) equals: #FooBar
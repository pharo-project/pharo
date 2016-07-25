running
setUpTrivialRequiresFixture
	self c3: (self 
				createClassNamed: #C3
				superclass: ProtoObject
				uses: { }).
	ProtoObject removeSubclass: self c3.
	self c3 superclass: nil.
	self c3 compile: 'foo ^self bla' classified: #accessing
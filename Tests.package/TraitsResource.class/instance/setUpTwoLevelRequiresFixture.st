running
setUpTwoLevelRequiresFixture
	self c4: (self 
				createClassNamed: #C4
				superclass: ProtoObject
				uses: { }).
	ProtoObject removeSubclass: self c4.
	self c4 superclass: nil.
	self c5: (self 
				createClassNamed: #C5
				superclass: self c4
				uses: { }).
	self c4 compile: 'foo ^self blew' classified: #accessing.
	self c5 compile: 'foo ^self blah' classified: #accessing
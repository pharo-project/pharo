running
setUpTranslatingRequiresFixture
	self c6: (self 
				createClassNamed: #C6
				superclass: ProtoObject
				uses: { }).
	ProtoObject removeSubclass: self c6.
	self c6 superclass: nil.
	self c7: (self 
				createClassNamed: #C7
				superclass: self c6
				uses: { }).
	self c8: (self 
				createClassNamed: #C8
				superclass: self c7
				uses: { }).
	self c6 compile: 'foo ^self x' classified: #accessing.
	self c7 compile: 'foo ^3' classified: #accessing.
	self c7 compile: 'bar ^super foo' classified: #accessing.
	self c8 compile: 'bar ^self blah' classified: #accessing
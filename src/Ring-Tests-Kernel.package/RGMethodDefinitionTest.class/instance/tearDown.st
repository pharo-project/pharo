running
tearDown
	self class removeSelectorSilently: #foo.
	self class removeProtocol: 'test'.
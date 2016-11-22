running
tearDown
	super tearDown.
	SystemAnnouncer announcer: oldSystemAnnouncer.
	self class environment at: #TestClass ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass1 ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass2 ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass3 ifPresent: [ :class | class removeFromSystem ]
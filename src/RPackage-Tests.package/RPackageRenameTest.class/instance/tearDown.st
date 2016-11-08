running
tearDown 
	super tearDown.
	SystemAnnouncer announcer: oldSystemAnnouncer.
	self class environment at: #TestClass ifPresent: [ :class | class removeFromSystem ].
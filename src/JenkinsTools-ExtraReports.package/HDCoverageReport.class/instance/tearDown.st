running
tearDown
	wrappers do: [ :each | each uninstall ].
	super tearDown.
	self generate
running
tearDown
	super tearDown.
	"Traits should be cleaned before classFactory because it seems class factory only knwos how to clean classes, not traits."
	self tearDownTraits.
	self tearDownClassFactory.
	self tearDownGlobalVariables.
	self streamFactory tearDown.
	self tearDownInstanceVariables.
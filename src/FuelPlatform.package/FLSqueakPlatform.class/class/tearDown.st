private-extensions-flserializationtest
tearDown
	<flExtensionOf: #FLSerializationTest>
	super tearDown.
	"Traits should be cleaned before classFactory because it seems class factory only knows how to clean classes, not traits."
	self tearDownTraits.
	self tearDownClassFactory.
	self tearDownGlobalVariables.
	self tearDownInstanceVariables
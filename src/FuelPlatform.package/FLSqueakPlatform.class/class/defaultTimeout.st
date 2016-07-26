private-extensions-flserializationtest
defaultTimeout
	<flExtensionOf: #FLSerializationTest>
	"We have some long running tests which will be terminated by Squeaks
	implementation of TestCase>>runCase. Increase the default fro 5 to 60 seconds"
	
	^ 60 "seconds"
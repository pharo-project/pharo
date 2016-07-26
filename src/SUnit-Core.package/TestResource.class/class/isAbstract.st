testing
isAbstract
	"Override to true if a TestResource subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestResource
			
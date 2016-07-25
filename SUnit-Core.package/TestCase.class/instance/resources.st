accessing
resources
	"We give TestCase this instance-side method so that methods polymorphic with TestSuite can be code-identical.  Having this instance-side method also helps when writing tests of resource behaviour. Except for such tests, it is rare to override this method and should not be done without thought.  If there were a good reason why a single test case needed to share tests requiring different resources, it might be legitimate."

	^self class resources
Normally a test will set up all the objects it needs and tear them down again after it has run.  This self-containedness makes a test more robust.  Use TestResources only for objects that are needed by several tests and that are too 'expensive' (in time or otherwise) to recreate and destroy for each test.  A viable approach is to develop the code in MyTestCase's #setUp and #tearDown methods, then at some point refactor the code into the #setUp and #tearDown of a TestResource whose class is added to MyTestCase class>>resource method.

TestResource uses the singleton pattern.  A TestResource class will set up a single instance of itself when first requested and tear it down again at the end of TestSuite>>run (or TestCase>>run, >>debug and >>debugAsFailure).  Normally, a TestResource, once setUp, remains active during the running of all remaining tests and is #reset after all tests have run.  For an exception, see subclass CompetingResource in SUnitResourcePatterns.  Users can choose to #reset a resource in the #tearDown of a test that alters it, sacrificing the performance gain of having a single #setUp of the resource for the certainty that other tests using it will not see the alterations.  Generally however, this should be the exception:  if you need to reset the resource for every test that uses it, its code should just be part of your test's #setUp and #tearDown code.

To use, create a subclass of TestResource and override the following:
	- TestCase class>>resources, to return a collection including the TestResource class, for all test case classes that need it
		* a TestCase' resources are set up in the order returned and torn down in the reverse order
	- TestResource class>>resources, if the resource itself always needs some other resource to be present before it can set up
		* a TestResource's resource are set up before it and torn down after it, and are set up in the order returned and torn down in the reverse order
	- TestResource>>setUp and tearDown, to define initial and final behaviour (just like a test)
	- TestResource>>isAvailable, to return true if it is and false if it isn't (the framework calls this after setUp);  ideally, this call should not change the resource' state - that should be done in setUp

TestResource implements the singleton pattern in its class-side #isAvailable and #reset methods.  Do not override these when creating specific resources;  unless you are developing a whole new pattern of use, it will always be correct to override instance-side #setUp, #tearDown and #isAvailable, and dangerous to override class>>isAvailable, class>>isAlreadyAvailable and class>>reset.

Generally, users do not code interactions with a test's resources during the running of a test.  Code that reads a resource' values while leaving its state strictly alone is safe enough.  A test must leave a resource in a clean state:  always use #reset if a test must protect later-running tests from unsafe changes (and review whether in such a case a resource is the right thing to use in the first place).

See my superclass' comment for assertion and logging information.

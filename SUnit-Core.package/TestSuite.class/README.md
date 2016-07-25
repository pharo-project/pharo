This is a Composite of Tests, either TestCases or other TestSuites. The top-level protocol is #run.  This creates aTestResult and sends
	self run: aTestResult.
then ensures that any TestResources made available during the run are reset.  These, and the dependencies protocol, are common between this and TestCase.
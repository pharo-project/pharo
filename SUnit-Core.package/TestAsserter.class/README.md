I understand methods beginning #assert:... and #deny:... (and my class-side understands #assert:description:).  I am the superclass of TestCase and TestResource and can also be the superclass of any test helper classes you create to factor out test behaviour.  I exist so that test code can be refactored between my subclasses without difficulty.

Send #assert:description: when you want to check for an expected value. For example, you might say
	self assert: socket isOpen description: 'We requested a socket but now it is not open'.
to test whether or not a socket is open at a point in a test.  Use description strings both to give more information about where a test failed in debugger notifiers and logs, and to document the intent of a test.  Other methods include #assert:, #assert:description:resumable:, #deny:, #deny:description:, #deny:description:resumable:, #should:raise:, #should:raise:description:, #shouldnt:raise:, #shouldnt:raise:description:.  All these methods are defined on the superclass, TestAsserter.  (Any convenience assertion methods you create for general use should also be defined in my 'convenience' protocol.)

Override my class-side #isLogging in subclasses to have failed assertion descriptions shown on the Transcript.  To have them appear elsewhere, also override my class-side #failureLog.

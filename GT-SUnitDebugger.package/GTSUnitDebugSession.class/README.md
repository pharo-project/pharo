I am a debugging session for the SUnit debugger. 
I extract from the execution stack various data needed by the user interface of the SUnit debugger, like the test object and method.

Public API and Key Messages

- process:context: does the initialization of the session
- data is provided using the methods in the accessing protocol

    Instance Variables
	actualResult:		<Object>
	assertionContext:		<Object>
	expectedResult:		<Object>
	testObject:		<Object>
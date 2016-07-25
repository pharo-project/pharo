A DebugContext is a helper that complements DebugSession. It is meant to be created dynamically on a context when the session wants to access the provided services.

To create instances first call forContext: to set the current context, and then if
the interrupted is different use topContext:

Not sure if it is still a good idea to have this class. 

Instance Variables
	context:		<Object>
	method:		<Object>
	methodNode:		<Object>
	ranges:		<Object>
	topContext:		<Object>

context
	- xxxxx

method
	- xxxxx

methodNode
	- xxxxx

ranges
	- xxxxx

topContext
	- xxxxx

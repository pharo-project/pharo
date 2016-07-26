I am the ultimate error. 
By default if an Error or Exception is not handled by the code the default action is to raise an UnhandledError which in interactive mode triggers the UIManager to open a debugger.

	Error signal
	...
	UnhandledError signalForException: error
	...
	UIManager opens a debugger
	
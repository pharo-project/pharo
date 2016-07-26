A DebugSession models a debuggeing session. It contains the interrupted context and process. Its main goal is to handle debugger actions such as restart or stepInto, as well as recomplilation of methods. It is the model used as an input to a ui.

As it is just a model it does now contain any information related to the ui. For example, it does not know what a selection in the ui is. It is the job on the ui to maintain the selection and call this session with the propper context.

To create sessions use the mehod 'process: aProcess context: aContext'. aContext must be a context belonging to aProcess, and aProcess must be an interrupted process.

Instance Variables
	context:		<Object>
	errorWasInUIProcess:		<Object>
	process:		<Object>

context
	- xxxxx

errorWasInUIProcess
	- xxxxx

process
	- xxxxx

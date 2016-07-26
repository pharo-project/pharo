I provide the basic functionality needed to create new debuggers. 
Subclasses should override the following methods:
	debuggerStructureIn: -> provides the structure of the debugger
	debuggerTransmissionsIn: -> creates transmissions between the widgets of a debuggers
	selectedContext 
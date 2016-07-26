*GT-Debugger-UI
gtDebuggerRawIn: composite
	
	(GTObjectVariablesBrowser new variablesIn: composite)
		title: 'Raw';
		send: #value
*GT-Debugger-UI
gtDebuggerRawIn: composite
	<gtDebuggerPresentationOrder: 15>
	
	(GTProtoObjectVariablesBrowser new variablesIn: composite)
		title: 'Raw';
		send: #value
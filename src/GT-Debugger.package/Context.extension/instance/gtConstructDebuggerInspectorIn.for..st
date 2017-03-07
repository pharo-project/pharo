*GT-Debugger
gtConstructDebuggerInspectorIn: composite for: aDebugger
	
	^ composite custom: ((GTDebuggerInspector new  
		debugger: aDebugger;
		notifyPaneRemoved: true;
		showFirst: [ :a | a custom: (aDebugger class variablesBrowserClass new 
			debugger: aDebugger;
			startOn: self;
			restoreSettings;
			yourself).];
		yourself) 
			startOn: self)
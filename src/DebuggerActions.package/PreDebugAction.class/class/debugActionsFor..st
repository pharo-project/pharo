actions creation
debugActionsFor: aDebugger
	<preDebuggingAction>
	
	^ { 
		self new
			id: #abandonAction;
			selector: #close;
			order: 10;
			label: 'Abandon'.
		self new
			id: #openFullDebuggerAction;
			selector: #openFullDebugger;
			order: 15;
			label: 'Debug'
	}
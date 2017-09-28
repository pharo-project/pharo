initialization
initialize
	super initialize.
	builder := ShiftClassBuilder new.
	builder buildEnvironment: ShSmalltalkGlobalsEnvironment new.	
	
	
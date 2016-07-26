building
debuggerStructureIn: browser
	
	browser
		row: #stack;
		row: [ :c | c
			column: #code; 
			column: #bytecode ] span: 2;
		row: #inspector.
		
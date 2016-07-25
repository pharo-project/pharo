I provide a simple interface for constructing an IRMethod.  For example, to create an ir method that compares first instVar to first arg and returns 'yes' or 'no' (same example as in BytecodeGenerator), do:

	IRBuilder new
		numArgs: 1;
		addTemps: #(a z);
		pushReceiver;
		pushInstVar: 1;
		pushTemp: #a;
		send: #>;
		jumpAheadTo: #else if: false;
		pushLiteral: 'yes';
		returnTop;
		jumpAheadTarget: #else;
		pushLiteral: 'no';
		returnTop;
		ir

Sending #compiledMethod to an ir method will generate its compiledMethod.  Sending #methodNode to it will decompile to its parse tree.

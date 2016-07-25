code generation
emitValue: methodBuilder

		methodBuilder
			pushInstVar: baseSlot index;
			pushLiteral: offset;
			send: #bitAt:;
			pushLiteral: 1;
			send: #==.
	
	
code generation
emitValue: methodBuilder

		methodBuilder
			pushInstVar: baseSlot index;
			pushLiteral: self name;
			pushLiteral: nil;
			send: #at:ifAbsent:
	
	
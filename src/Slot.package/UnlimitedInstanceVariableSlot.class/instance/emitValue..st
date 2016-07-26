code generation
emitValue: methodBuilder

		methodBuilder
			pushInstVar: baseSlot index;
			pushLiteral: offset;
			send: #at:
			
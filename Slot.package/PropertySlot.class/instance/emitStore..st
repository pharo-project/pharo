code generation
emitStore: methodBuilder
	| tempName |

	tempName := Object new.
	methodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: self name;
		pushTemp: tempName;
		send: #at:put:
	
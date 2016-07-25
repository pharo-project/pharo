code generation
emitStore: methodBuilder
	| tempName |

	tempName := Object new.
	methodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: offset;
		pushTemp: tempName;
		send: #at:put:
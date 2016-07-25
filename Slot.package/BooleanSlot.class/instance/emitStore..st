code generation
emitStore: methodBuilder
	| tempName |

	tempName := Object new.
	methodBuilder
		send: #asBit;
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: offset;
		pushTemp: tempName;
		send: #bitAt:put:;
		storeInstVar: baseSlot index
	
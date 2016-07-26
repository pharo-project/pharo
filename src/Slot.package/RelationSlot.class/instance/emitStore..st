code generation
emitStore: aMethodBuilder
	| tempName |
	tempName := Object new.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushReceiver;
		pushLiteralVariable: #slot -> self;
		pushTemp: tempName;
		send: #writeSlot:value:
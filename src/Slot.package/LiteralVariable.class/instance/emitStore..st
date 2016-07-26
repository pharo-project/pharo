code generation
emitStore: aMethodBuilder
	| tempName |
	tempName := Object new.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushLiteralVariable: #global -> self;
		pushTemp: tempName;
		send: #write:
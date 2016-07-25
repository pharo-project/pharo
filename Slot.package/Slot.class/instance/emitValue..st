code generation
emitValue: aMethodBuilder
	aMethodBuilder
		pushLiteralVariable: #slot -> self;
		pushReceiver;
		send: #read:
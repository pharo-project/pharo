examples
pushClosureCopyOneCopiedTemp

	^IRBuilder new
		addTemps: #(a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(a) args: #() jumpTo: #block;
		addTemps: #(d);  "the temp"
		pushTemp: #a;
		pushLiteral: 1;
		send: #+;
		storeTemp:  #d;
		popTop;
		pushTemp: #d;
		blockReturnTop;
		jumpAheadTarget: #block;	
		send: #value;
		returnTop;
		ir.

examples
pushClosureCopyOneCopiedArg
	^ IRBuilder new
		addTemps: #(#a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(#a) args: #(#d) jumpTo: #block;
		pushTemp: #a;
		pushTemp: #d;
		send: #+;
		blockReturnTop;
		jumpAheadTarget: #block;
		pushLiteral: 1;
		send: #value:;
		returnTop;
		ir
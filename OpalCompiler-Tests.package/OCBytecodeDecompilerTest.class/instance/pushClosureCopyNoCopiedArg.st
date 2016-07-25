examples
pushClosureCopyNoCopiedArg
	^ IRBuilder new
		pushReceiver;
		pushClosureCopyCopiedValues: #() args: #(#d) jumpTo: #block;
		pushLiteral: 1;
		pushTemp: #d;
		send: #+;
		blockReturnTop;
		jumpAheadTarget: #block;
		pushLiteral: 1;
		send: #value:;
		returnTop;
		ir
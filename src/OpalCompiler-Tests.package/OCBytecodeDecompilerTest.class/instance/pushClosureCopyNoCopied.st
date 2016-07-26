examples
pushClosureCopyNoCopied
	^ IRBuilder new
		pushReceiver;
		pushClosureCopyCopiedValues: #() args: #() jumpTo: #block;
		pushLiteral: 1;
		pushLiteral: 2;
		send: #+;
		blockReturnTop;
		jumpAheadTarget: #block;
		send: #value;
		returnTop;
		ir
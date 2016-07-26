private
sawClosureBytecode
	constructor isForClosures ifFalse:
		[constructor primitiveChangeClassTo: DecompilerConstructorForClosures new]
instruction decoding
blockReturnConstant: cst

	self addBytecode: 'blockReturn: ' , cst printString
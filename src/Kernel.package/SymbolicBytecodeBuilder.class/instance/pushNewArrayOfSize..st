instruction decoding
pushNewArrayOfSize: numElements 
	self addBytecode: 'push: (Array new: ', numElements printString, ')'
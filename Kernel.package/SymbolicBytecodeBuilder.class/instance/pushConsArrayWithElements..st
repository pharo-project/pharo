instruction decoding
pushConsArrayWithElements: numElements 
	self addBytecode: 'pop ', numElements printString, ' into (Array new: ', numElements printString, ')'
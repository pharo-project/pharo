instruction decoding forward
blockReturnConstant: value 
	self pushConstant: value.
	self blockReturnTop
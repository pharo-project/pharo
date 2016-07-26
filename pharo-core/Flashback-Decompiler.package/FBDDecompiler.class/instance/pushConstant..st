data flow instructions
pushConstant: value
	simulatedStack addLast: (builder codeLiteral: value)
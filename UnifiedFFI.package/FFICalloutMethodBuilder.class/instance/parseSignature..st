private
parseSignature: aSignature
	^ FFIFunctionParser new  
		requestor: self requestor;
		parseNamedFunction: aSignature
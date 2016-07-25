private-replacing
replaceSourceWith: aNode 
	self addReplacement: (RBStringReplacement 
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
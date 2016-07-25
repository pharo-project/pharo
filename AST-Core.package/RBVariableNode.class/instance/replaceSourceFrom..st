replacing
replaceSourceFrom: aNode 
	self addReplacement: (RBStringReplacement 
				replaceFrom: aNode start
				to: aNode stop
				with: self name)
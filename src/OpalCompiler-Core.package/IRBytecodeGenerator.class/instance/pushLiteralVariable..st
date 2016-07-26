instructions
pushLiteralVariable: object 
	stack push.
	encoder genPushLiteralVar: (self literalIndexOf: object).
accessing
sender: aSenderContext
	| nArgs |
	self requestor: aSenderContext method methodClass.
	nArgs := aSenderContext method numArgs.
	methodArgs := aSenderContext method ffiArgumentNames.
	self receiver: aSenderContext receiver.
	self assert: (methodArgs size = nArgs).

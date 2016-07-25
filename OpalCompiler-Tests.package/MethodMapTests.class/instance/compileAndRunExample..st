util
compileAndRunExample: aSelector
	| cm |
	
	cm := self compileMethod:  MethodMapExamples>>aSelector.
	^cm valueWithReceiver:  MethodMapExamples new arguments: #().
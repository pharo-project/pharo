*OpalCompiler-Core
errorMethodNode: selector errorMessage: messageText	
	| message |
	message := RBMessageNode 
		receiver: (RBSelfNode named: 'self')
		selector: #error: 
		arguments: {RBLiteralNode value: messageText}.
	^ self 
		selector: selector 
		arguments: ((1 to: selector numArgs) collect: [ :i | RBArgumentNode named: 't' , i asString ]) 
		body: (RBSequenceNode statements: {message}) 
public access
evaluate: textOrString 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString in: context to: receiver notifying: requestor ifFail: self compilationContext failBlock logged: self compilationContext logged
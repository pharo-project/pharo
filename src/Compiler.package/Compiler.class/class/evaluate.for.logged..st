evaluating
evaluate: textOrString for: anObject logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag
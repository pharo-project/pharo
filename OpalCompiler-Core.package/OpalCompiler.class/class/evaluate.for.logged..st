old - public
evaluate: textOrString for: anObject logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self new
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		evaluate
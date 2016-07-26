old - public
evaluate: textOrString logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^ self new
		source: textOrString;
		logged: logFlag;
		evaluate
	
			
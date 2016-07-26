old - public
evaluate: textOrString notifying: aController logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^ self new
		source: textOrString;
		logged: logFlag;
		requestor: aController;
		evaluate
	
			
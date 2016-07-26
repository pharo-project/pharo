evaluating
evaluate: textOrString notifying: aController logged: logFlag 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController logged: logFlag
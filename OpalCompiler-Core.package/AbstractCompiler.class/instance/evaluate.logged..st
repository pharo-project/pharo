old - public
evaluate: textOrString logged: logFlag 

	^ self
		source: textOrString;
		logged: logFlag;
		evaluate
	
			
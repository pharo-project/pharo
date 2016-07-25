old - public
evaluate: textOrString notifying: aController logged: logFlag 

	^ self
		source: textOrString;
		logged: logFlag;
		requestor: aController;
		evaluate
	
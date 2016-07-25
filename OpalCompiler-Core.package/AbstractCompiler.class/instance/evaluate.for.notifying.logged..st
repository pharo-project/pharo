old - public
evaluate: textOrString for: anObject notifying: aController logged: logFlag

	^ self 
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		requestor: aController;
		evaluate
	
			
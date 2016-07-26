old - public
evaluate: aString in: aContext to: aReceiver
	
	^self
		source: aString;
		context: aContext;
		receiver: aReceiver;
		failBlock: [^ #failedDoit];
		evaluate
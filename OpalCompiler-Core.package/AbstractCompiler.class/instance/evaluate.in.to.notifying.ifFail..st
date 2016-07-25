old - public
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: aFailBlock 
	
	^self
		source: textOrStream;
		context: aContext;
		receiver: aReceiver;
		requestor: aRequestor;
		failBlock: aFailBlock;
		evaluate
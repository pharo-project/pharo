old - public
compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: aFailBlock

	^self 
		source: textOrStream;
		class: aClass;
		context: aContext;
		requestor: aRequestor;
		noPattern: true;
		failBlock: aFailBlock;
		translate
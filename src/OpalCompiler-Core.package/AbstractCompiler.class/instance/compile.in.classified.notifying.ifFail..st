old - public
compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail:  aFailBlock 

	^self 
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		failBlock: aFailBlock;
		translate
old - public
parse: textOrStream in: aClass notifying: req

	^self 
		source: textOrStream;
		class: aClass;
		requestor: req;
		translate.
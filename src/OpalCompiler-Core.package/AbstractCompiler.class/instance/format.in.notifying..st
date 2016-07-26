old - public
format: textOrStream in: aClass notifying: aRequestor
	^self
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		format
		
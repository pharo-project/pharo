old - public
format: textOrStream in: aClass notifying: aRequestor
	^self new
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		format
		
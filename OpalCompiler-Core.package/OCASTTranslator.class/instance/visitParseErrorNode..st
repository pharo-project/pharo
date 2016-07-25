visitor-double dispatching
visitParseErrorNode: anErrorNode  
	methodBuilder 
		pushLiteralVariable: #error -> anErrorNode asSyntaxErrorNotification;
		send: #signal.


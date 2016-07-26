running
evaluate: aString in: aContext to: anObject
	^self class compiler source: aString;
		context: aContext;
		receiver: anObject;
		evaluate
old - public
parse: aString class: aClass noPattern: aBoolean context: aContext notifying: req ifFail: aBlock
	"Backwards compatibilty"
	
	^self 
		source: aString;
		class: aClass;
		noPattern: aBoolean;
		context: aContext;
		requestor: req;
		failBlock: aBlock;
		translate.
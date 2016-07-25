evaluating actions
evaluate: expression
	^ Smalltalk compiler
		source: expression;
		context: context;
		receiver: context receiver;
		evaluate
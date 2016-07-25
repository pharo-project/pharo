compiling
compile: codeString for: anObject in: evalContext

	| methodNode |
	methodNode := Smalltalk compiler
		source: ('self runBlock: [', codeString, ']');
		class: anObject class;
		context: evalContext;
		requestor: self;
		noPattern: true;
		failBlock: [^nil];
		translate.
		
	^ methodNode generateWithSource.
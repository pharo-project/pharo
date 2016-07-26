running
compileWithFailBlock: aBlock
	^ OpalCompiler new 
		source: text;
		class: MockForCompilation;
		requestor: self;
		failBlock: aBlock;
		translate.

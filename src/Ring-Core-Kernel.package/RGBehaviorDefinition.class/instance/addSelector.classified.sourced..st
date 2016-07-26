adding/removing methods
addSelector: selectorName classified: protocolName sourced: source

	self addMethod: ((RGMethodDefinition named: selectorName) 
							parent: self;
							protocol: protocolName;
							sourceCode: source;
							yourself)
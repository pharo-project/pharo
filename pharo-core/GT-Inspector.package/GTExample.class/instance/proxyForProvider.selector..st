private
proxyForProvider: aProvider selector: aSelector
	^ GTExampleProxy new
		owner: self;
		provider: aProvider;
		selector: aSelector asSymbol;
		yourself
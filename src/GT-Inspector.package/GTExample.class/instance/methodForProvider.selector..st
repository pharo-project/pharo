private
methodForProvider: aProvider selector: aSelector
	^ GTExampleProviderMethod new
		provider: aProvider;
		selector: aSelector asSymbol;
		yourself
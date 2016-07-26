instance creation
ruleFor: aClass protocol: aProtocol 
	^self
		rules: (((RBBrowserEnvironment new selectorsFor: aProtocol asSymbol in: aClass class)
				collect: [:selector | aClass perform: selector])
					asSortedCollection: [:a :b | a name < b name])
		name: ((aProtocol asString copy) at: 1 put: aProtocol first asUppercase;
				yourself)
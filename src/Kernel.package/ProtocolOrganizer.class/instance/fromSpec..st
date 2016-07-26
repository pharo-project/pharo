importing
fromSpec: aSpec
	
	aSpec do: [:spec || name methods |
		name := spec first asSymbol.
		methods := spec allButFirst asSet.
		self addProtocol: (Protocol 
								name: name
								methods: methods) ]
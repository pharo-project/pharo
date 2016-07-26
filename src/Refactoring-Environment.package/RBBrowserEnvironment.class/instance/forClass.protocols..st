environments
forClass: aClass protocols: protocolCollection 
	^RBProtocolEnvironment onEnvironment: self
		class: aClass
		protocols: protocolCollection
refactory-changes
compile: source in: class classified: aProtocol 
	^ self addChange: (RBRefactoryChangeManager changeFactory addMethodSource: source in: class classified: aProtocol)
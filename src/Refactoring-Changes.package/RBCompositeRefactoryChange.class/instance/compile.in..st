refactory-changes
compile: source in: class 
	^ self addChange: (RBRefactoryChangeManager changeFactory addMethodSource: source in: class)
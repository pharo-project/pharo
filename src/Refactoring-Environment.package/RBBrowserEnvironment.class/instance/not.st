environments
not
	self isSystem ifTrue: [^RBSelectorEnvironment new].
	^RBNotEnvironment onEnvironment: self
actions
removeOutgoingDependencies
	"Remove all dependencies to packages not included in this relation"
	
	self includedPackages
		do: [ :each | self removeOutgoingDependencies: each ]
		displayingProgress: 'Removing outgoing Dependencies'
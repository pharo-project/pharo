actions
removeOutgoingDependencies
	self packages do: [ :package | self removeOutgoingDependencies: package ].
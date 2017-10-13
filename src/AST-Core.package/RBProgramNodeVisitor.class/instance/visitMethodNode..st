visiting
visitMethodNode: aMethodNode 
	self visitArgumentNodes: aMethodNode arguments.
	aMethodNode pragmas
		do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
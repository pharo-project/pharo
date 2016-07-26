visitor-double dispatching
visitMethodNode: aMethodNode
	aMethodNode arguments: (self visitArgumentNodes: aMethodNode arguments).
	aMethodNode pragmas: (aMethodNode pragmas collect: [ :each | self visitNode: each ]).
	aMethodNode body: (self visitNode: aMethodNode body)
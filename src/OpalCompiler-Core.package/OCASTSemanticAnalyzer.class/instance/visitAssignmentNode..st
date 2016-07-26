visitor
visitAssignmentNode: anAssignmentNode
	| var |
	self visitNode: anAssignmentNode value.
	var := (self lookupVariableForWrite: anAssignmentNode variable)
		ifNil: [ self undeclaredVariable: anAssignmentNode variable ].
	scope isInsideOptimizedLoop
		ifTrue: [ var markRepeatedWrite ]
		ifFalse: [ var markWrite ].
	anAssignmentNode variable binding: var
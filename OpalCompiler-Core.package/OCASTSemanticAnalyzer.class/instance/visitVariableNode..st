visitor
visitVariableNode: aVariableNode
	| var |
	var := (self lookupVariableForRead: aVariableNode) ifNil: [(self undeclaredVariable: aVariableNode)].
	(var isTemp and: [var isEscaping and: [scope outerScope isInsideOptimizedLoop]])
		ifTrue: [
			 "only variables written within the loop needs to be marked as escaping write"
			var isRepeatedWrite ifTrue: [var markEscapingWrite].
			var isRead ifTrue: [var markEscapingRead]].
	aVariableNode binding: var.
	var isUninitialized ifTrue: [self uninitializedVariable: aVariableNode].
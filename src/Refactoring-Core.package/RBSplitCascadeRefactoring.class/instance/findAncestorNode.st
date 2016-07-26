preconditions
findAncestorNode
	"The ancestor node is the node that is contained within the sequence. In most cases this is the cascade itself, but it also can be an assignment or a return node."

	ancestorNode := cascadeNode.
	[ ancestorNode parent isSequence not and: [ ancestorNode parent isAssignment ] ]
		whileTrue: [ ancestorNode := ancestorNode parent ].
	[ ancestorNode parent isSequence not and: [ ancestorNode parent isReturn ] ]
		whileTrue: [ ancestorNode := ancestorNode parent ].
	ancestorNode parent isSequence
		ifFalse: [ self refactoringFailure: 'To split this cascade, you must extract it to a temporary first' ]
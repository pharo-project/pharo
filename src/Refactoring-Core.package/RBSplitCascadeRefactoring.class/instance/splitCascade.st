transforming
splitCascade
	ancestorNode parent
		addNode: (beforeNodes size > 1
			ifTrue: [ RBCascadeNode messages: beforeNodes ]
			ifFalse: [ beforeNodes first ])
		before: ancestorNode.
	afterNodes size > 1
		ifTrue: [ cascadeNode messages: afterNodes ]
		ifFalse: [ cascadeNode replaceWith: afterNodes first ].
	class compileTree: ancestorNode methodNode
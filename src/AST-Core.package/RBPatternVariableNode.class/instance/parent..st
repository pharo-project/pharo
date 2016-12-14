accessing
parent: aBRProgramNode 
	"Fix the case where '``@node' should match a single node, not a sequence node."

	super parent: aBRProgramNode.
	parent isDynamicArray
		ifTrue: [ self isStatement
				ifFalse: [ isList := false ] ].
	parent isSequence 
		ifTrue: 
			[(self isStatement or: [parent temporaries includes: self]) 
				ifFalse: [isList := false]]
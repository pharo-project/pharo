testing
isDirectlyUsed
	"This node is directly used as an argument, receiver, or part of an assignment."

	^parent ifNil: [false] ifNotNil: [parent directlyUses: self]
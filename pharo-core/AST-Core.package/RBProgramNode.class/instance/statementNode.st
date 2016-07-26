querying
statementNode
	"Return your topmost node that is contained by a sequence node."

	(parent isNil or: [parent isSequence]) ifTrue: [^self].
	^parent statementNode
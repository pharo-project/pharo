macro transformations
toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: (Array with: variable).
	variable scope: -1.
	variable beBlockArg.
	^MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence
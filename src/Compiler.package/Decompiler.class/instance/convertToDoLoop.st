private
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| initStmt toDoStmt limitStmt |
	statements size < 2 ifTrue: [^ self].
	initStmt := statements at: statements size-1.
	(toDoStmt := statements last toDoFromWhileWithInit: initStmt)
		== nil ifTrue: [^ self].
	initStmt variable scope: -1.  "Flag arg as block temp"
	statements removeLast; removeLast; addLast: toDoStmt.

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...]"
	statements size < 2 ifTrue: [^ self].
	limitStmt := statements at: statements size-1.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first
		and: [self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset]]])
		ifFalse: [^ self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements removeLast; removeLast; addLast: toDoStmt.


visiting
argumentsInEvaluationOrder
	"Answer the receivers arguments in evaluation order.
	 If the receiver is a transformed to:do: node this will undo the misordering done by the transformation."
	^(special > 0
	   and: [(MacroTransformers at: special) == #transformToDo:
	   and: [arguments size >= 7]])
		"arguments are in a weid order and may be nil in a transformed to:do: loop.  sigh...
		 c.f. emitCodeForToDo:encoder:value:"
		ifTrue:
			[(arguments at: 7)	"limitInit"
				ifNil: [{	(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]
				ifNotNil: [:limitInit|
						{ limitInit.
						(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]]
		ifFalse:
			[arguments]
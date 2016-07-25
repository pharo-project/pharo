initialize
initialize
	"OCASTTranslator initialize"
	
	super initialize.
	
	OptimizedMessages := { 
	#caseOf: -> #emitCaseOf: .
	#caseOf:otherwise: -> #emitCaseOfOtherwise: .
	#ifFalse: -> #emitIfFalse: .
	#ifFalse:ifTrue: -> #emitIfFalseIfTrue: .
	#ifNil: -> #emitIfNil: .
	#ifNil:ifNotNil: -> #emitIfNilIfNotNil: .
	#ifNotNil: -> #emitIfNotNil: .
	#ifNotNil:ifNil: -> #emitIfNotNilIfNil: .
	#ifTrue: -> #emitIfTrue: .
	#ifTrue:ifFalse: -> #emitIfTrueIfFalse: .
	#or: -> #emitOr: .
	#and: -> #emitAnd: .
	#timesRepeat: -> #emitTimesRepeat: .
	#repeat -> #emitRepeat: .
	#to:by:do: -> #emitToByDo: .
	#to:do: -> #emitToDo: .
	#whileFalse: -> #emitWhileFalse: .
	#whileTrue: -> #emitWhileTrue: .
	#whileFalse -> #emitWhileFalse: .
	#whileTrue -> #emitWhileTrue:
	 } asDictionary
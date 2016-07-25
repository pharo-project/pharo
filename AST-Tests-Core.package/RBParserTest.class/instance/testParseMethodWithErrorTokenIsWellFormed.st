tests parsing
testParseMethodWithErrorTokenIsWellFormed
	| node strangeMethod body statement message errorNode |
	
	strangeMethod := '
	selector  
		|temp| 
		temp := ''this is right'', ''wrong because no end.
		^temp'.
	node := self parseFaultyMethod: strangeMethod.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assert: node arguments isEmpty.

	body := node body.
	self assert: body isSequence.
	self assert: body isFaulty.
	self assert: (body temporaries includes: (RBVariableNode named: 'temp')).

	statement := body statements first.
	self assert: statement isFaulty.
	self assert: statement isAssignment .
	
	message := statement value.
	self assert: message isFaulty.
	self assert: message arguments size equals: 1.

	errorNode := message arguments at: 1.
	self assert: errorNode isFaulty.
	self assert: errorNode value equals: 'wrong because no end.
		^temp'.
	self assert: errorNode errorMessage equals: 'Unmatched '' in string literal.' translated.
	
	
	
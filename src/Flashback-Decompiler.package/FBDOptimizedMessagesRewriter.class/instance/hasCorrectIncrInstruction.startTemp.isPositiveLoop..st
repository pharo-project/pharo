testing
hasCorrectIncrInstruction: msgNode startTemp: startTemp isPositiveLoop: isPositiveLoop
	| incrInstruction |
	incrInstruction := msgNode arguments first body statements last.
	incrInstruction isAssignment
		ifFalse: [ ^ false ].
	incrInstruction variable = startTemp
		ifFalse: [ ^ false ].
	incrInstruction value isMessage
		ifFalse: [ ^ false ].
	incrInstruction value selector = #+
		ifFalse: [ ^ false ].
	incrInstruction value receiver = startTemp
		ifFalse: [ ^ false ].
	incrInstruction value arguments first value isLiteral
		ifFalse: [ ^ false ].
	^ incrInstruction value arguments first value positive = isPositiveLoop
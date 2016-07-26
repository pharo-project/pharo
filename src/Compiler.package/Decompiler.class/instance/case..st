instruction decoding
case: dist
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns wierdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := (self popTo: stack removeLast) readStream.
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].
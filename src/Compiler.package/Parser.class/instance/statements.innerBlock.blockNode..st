expression types
statements: argNodes innerBlock: inner blockNode: theBlockNode

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						 stmts size = 0 ifTrue: 
							[stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	(inner and: [argNodes size > 0 and: [stmts isEmpty]]) ifTrue: [
		"A ST-80 empty block should return nil"
		stmts addLast: (encoder encodeVariable: 'nil' sourceRange: nil ifUnknown: [self error: 'Compiler internal error'])].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true
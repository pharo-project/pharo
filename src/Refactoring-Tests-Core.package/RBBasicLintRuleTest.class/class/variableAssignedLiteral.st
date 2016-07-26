miscellaneous
variableAssignedLiteral
	| detector |
	detector := self new.
	detector name: 'Variable is only assigned a single literal value'.
	detector result: nil references.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| defClass selector |
					(allSubclasses inject: 0
						into: 
							[:sum :class | 
							| sels |
							sels := class whichSelectorsWrite: each.
							sels size == 1 ifTrue: 
									[selector := sels asArray first.
									defClass := class].
							sum + sels size])
							== 1 ifTrue: 
								[| tree searcher |
								searcher := RBParseTreeSearcher new.
								searcher addSearch: (each , ' := ``@object')
											-> [:aNode :answer | answer isNil and: [aNode value isLiteralNode]].
								tree := defClass parseTreeFor: selector.
								tree notNil ifTrue: 
										[(searcher executeTree: tree initialAnswer: nil) == true
											ifTrue: [result addInstVar: each for: context selectedClass]]]]].
	^detector
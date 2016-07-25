miscellaneous
stringConcatenation
	| detector matcher concatenationMatcher |
	detector := self new.
	detector name: 'String concatenation instead of streams'.
	matcher := RBParseTreeSearcher new.
	concatenationMatcher := RBParseTreeSearcher new.
	concatenationMatcher
		addSearch: '`@receiver , `@argument' -> [:aNode :answer | true].
	matcher
		addSearches: #('``@collection do: [:`each | | `@temps | ``@.Statements]' '``@collection do: [:`each | | `@temps | ``@.Statements] separatedBy: [| `@temps1 | ``@.Statements1]' '``@number to: ``@endNumber do: [:`i | | `@temps | ``@.Statements]' '``@collection detect: [:`each | | `@temps | ``@.Statements]' '``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [| `@temps1 | ``@.Statements1]' '``@collection select: [:`each | | `@temps | ``@.Statements]' '``@collection inject: ``@value into: [:`each | | `@temps | ``@.Statements]')
				-> 
					[:aNode :answer | 
					answer or: 
							[(aNode arguments detect: 
									[:each | 
									each isBlock
										and: [concatenationMatcher executeTree: each initialAnswer: false]]
								ifNone: [nil]) notNil]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
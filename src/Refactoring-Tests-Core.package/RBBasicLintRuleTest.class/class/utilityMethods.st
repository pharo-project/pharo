miscellaneous
utilityMethods
	| detector |
	detector := self new.
	detector name: 'Utility methods'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass | (context selector numArgs == 0) or: 
					[(context protocols detect: 
							[:each | 
							(self utilityProtocols detect: [:protocol | protocol match: each]
								ifNone: [nil]) notNil]
						ifNone: [nil]) notNil])
				ifFalse: 
					[(self subclassOf: context selectedClass overrides: context selector)
						ifFalse: 
							[(context superMessages isEmpty and: [context selfMessages isEmpty]) ifTrue: 
									[(RBParseTreeSearcher
										references: context selectedClass allInstVarNames
												, context selectedClass allClassVarNames asArray , #('self')
										in: context parseTree) isEmpty
										ifTrue: [result addClass: context selectedClass selector: context selector]]]]].
	^detector
bugs
sentNotImplemented
	| detector |
	detector := self new.
	detector name: 'Messages sent but not implemented'.
	detector methodBlock: 
			[:context :result | 
			| message |
			message := context messages detect: [:each | (context implements: each) not]
						ifNone: [nil].
			message isNil ifTrue: 
					[message := context superMessages detect: 
									[:each | 
									context selectedClass superclass isNil
										or: [(context selectedClass superclass canUnderstand: each) not]]
								ifNone: [nil].
					message isNil ifTrue: 
							[message := context selfMessages
										detect: [:each | (context selectedClass canUnderstand: each) not]
										ifNone: [nil]]].
			message notNil ifTrue: 
					[result addSearchString: message.
					result addClass: context selectedClass selector: context selector]].
	^detector
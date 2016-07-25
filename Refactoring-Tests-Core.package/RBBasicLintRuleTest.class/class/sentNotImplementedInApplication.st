possible bugs
sentNotImplementedInApplication
	| detector |
	detector := self new.
	detector name: 'Messages sent but not implemented in application'.
	detector methodBlock: 
			[:context :result | 
			| message class block |
			message := context messages
						detect: [:each | (context isItem: each in: context application) not]
						ifNone: [nil].
			class := context selectedClass.
			block := 
					[:each | 
					| app |
					app := context application.
					(self canCall: each
						in: class
						from: app) not].
			message isNil ifTrue: [message := context selfMessages detect: block ifNone: [nil]].
			message isNil ifTrue: 
					[class := class superclass.
					class isNil
						ifTrue: 
							[context superMessages isEmpty ifFalse: [message := context superMessages asArray first]]
						ifFalse: [message := context superMessages detect: block ifNone: [nil]]].
			message notNil ifTrue: 
					[result addSearchString: message.
					result addClass: context selectedClass selector: context selector]].
	^detector
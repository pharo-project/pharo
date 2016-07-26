possible bugs
superSends
	| detector |
	detector := self new.
	detector name: 'Missing super sends'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass not
				and: [self superMessages includes: context selector]) ifTrue: 
						[(context selectedClass superclass notNil
							and: [context selectedClass superclass canUnderstand: context selector])
								ifTrue: 
									[(context superMessages includes: context selector)
										ifFalse: [result addClass: context selectedClass selector: context selector]]]].
	^detector
intention revealing
collectionMessagesToExternalObject
	| detector matcher |
	detector := self new.
	detector name: 'Sends add:/remove: to external collection'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: (#(#add: #remove: #addAll: #removeAll:)
				collect: [:each | ('(`@Object `@message: `@args) <1s> `@Arg' expandMacrosWith: each) asString])
					-> 
						[:aNode :answer | 
						answer or: 
								[(aNode receiver selector copyFrom: 1 to: (aNode receiver selector size min: 2))
									~= 'as' and: 
											[| receiver |
											receiver := aNode receiver receiver.
											receiver isVariable not or: 
													[((#('self' 'super') includes: receiver name)
														or: [Smalltalk includesKey: receiver name asSymbol]) not]]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
intention revealing
toDo
	| detector matcher |
	detector := self new.
	detector name: 'Uses to:do: instead of do:, with:do: or timesRepeat:'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '1 to: ``@object size do: [:`each | | `@temps | `@.Statements]' -> 
					[:aNode :answer | 
					answer or: 
							[| varName variableMatcher |
							varName := aNode arguments last arguments first name. "`each"
							variableMatcher := RBParseTreeSearcher new.
							variableMatcher addSearch: varName
										-> [:node :ans | ans and: [node parent isMessage and: [node parent selector == #at:]]].
							variableMatcher executeTree: aNode arguments last body initialAnswer: true]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
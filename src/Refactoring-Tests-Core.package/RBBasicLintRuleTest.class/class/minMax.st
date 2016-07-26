intention revealing
minMax
	| detector matcher |
	detector := self new.
	detector name: 'Uses ifTrue:/ifFalse: instead of min: or max:'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: #('(`x `message: `@y) `ifTrue: [`x := `@y]' '(`@x `message: `@y) `ifTrue: [`@x] `ifFalse: [`@y]')
				-> 
					[:aNode :answer | 
					answer or: 
							[(#(#ifTrue: #ifFalse: #ifTrue:ifFalse: #ifFalse:ifTrue:)
								includes: aNode selector)
									and: [#(#< #<= #> #>=) includes: aNode receiver selector]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
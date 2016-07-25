unnecessary code
endTrueFalse
	| detector matcher |
	detector := self new.
	detector name: 'Check for same statements at end of ifTrue:ifFalse: blocks'.
	matcher := (RBParseTreeSearcher new) addSearches: 
				#('`@object 
						ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
						ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' 
				'`@object 
						ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
						ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' 
				'`@object 
						ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
						ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' 
				'`@object 
						ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
						ifTrue: [| `@temps2 | `.Statement. `@.Statement2]') 
			-> [:aNode :answer | answer
						or: 
							[| node |
							node := aNode arguments first body statements last.
							(node isVariable and: [node = aNode arguments last body statements last]) not]]; yourself.
	detector methodBlock: [:context :result | (matcher executeTree: context parseTree initialAnswer: false)
			ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
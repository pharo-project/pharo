intention revealing
assignmentInIfTrue
	^self createParseTreeRule: 
			#('`@boolean 
					ifTrue: [| `@temps1 | `@.Statements1. `var := `@object1] 
					ifFalse: [| `@temps2 | `@.Statements2. `var := `@object2]' 
			'`@boolean 
					ifFalse: [| `@temps1 | `@.Statements1. `var := `@object1] 
					ifTrue: [| `@temps2 | `@.Statements2. `var := `@object2]') 
		name: 'Assignment to same variable and end of ifTrue:ifFalse: blocks'
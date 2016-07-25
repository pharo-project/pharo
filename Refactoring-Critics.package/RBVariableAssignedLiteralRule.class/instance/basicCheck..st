running
basicCheck: aClass 
	^ aClass instVarNames anySatisfy:  
		[ :each | 
		| defClass selector |
		(aClass withAllSubclasses 
			inject: 0
			into: 
				[ :sum :class | 
				| sels |
				sels := class whichSelectorsAssign: each.
				sels size == 1 ifTrue: 
					[ selector := sels asArray first.
					defClass := class ].
				sum + sels size ]) == 1 and: 
			[ | tree searcher |
			searcher := RBParseTreeSearcher new.
			searcher 
				matches: each , ' := ``@object'
				do: [ :aNode :answer | answer isNil and: [ aNode value isLiteralNode ] ].
			tree := defClass parseTreeFor: selector.
			tree notNil and: 
				[ (searcher 
					executeTree: tree
					initialAnswer: nil) == true ] ] ]
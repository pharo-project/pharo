running
checkClass: aClass 
	| allSubclasses |
	allSubclasses := aClass withAllSubclasses.
	aClass instVarNames do: 
		[ :each | 
		| defClass selector |
		(allSubclasses 
			inject: 0
			into: 
				[ :sum :class | 
				| sels |
				sels := class whichSelectorsAssign: each.
				sels size == 1 ifTrue: 
					[ selector := sels asArray first.
					defClass := class ].
				sum + sels size ]) == 1 ifTrue: 
			[ | tree searcher |
			searcher := RBParseTreeSearcher new.
			searcher 
				matches: each , ' := ``@object'
				do: [ :aNode :answer | answer isNil and: [ aNode value isLiteralNode ] ].
			tree := defClass parseTreeFor: selector.
			tree notNil ifTrue: 
				[ (searcher 
					executeTree: tree
					initialAnswer: nil) == true ifTrue: 
					[ result 
						addClass: aClass
						instanceVariable: each ] ] ] ]
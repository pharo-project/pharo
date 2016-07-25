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
				sels := class whichSelectorsAccess: each.
				sels size == 1 ifTrue: 
					[ selector := sels asArray first.
					defClass := class ].
				sum + sels size ]) == 1 and: 
			[ | tree |
			tree := defClass parseTreeFor: selector.
			tree notNil and: 
				[ (RBReadBeforeWrittenTester 
					isVariable: each
					writtenBeforeReadIn: tree) ] ] ]
tests interval
testIntervals
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			(each parent isNil or: [each parent isCascade not and: [ each parent isLiteralNode not]]) 
				ifTrue: 
					[| newNode source |
					source := tree source copyFrom: each start to: each stop.
					each isPragma ifFalse: [
						newNode := each isMethod 
								ifTrue: [self parserClass parseMethod: source]
								ifFalse: [self parserClass parseExpression: source].
						self compare: each to: newNode]]]
tests interval
testBestNodeFor
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			each sourceInterval isEmpty 
				ifFalse: 
					[self assert: ((tree bestNodeFor: each sourceInterval) = each 
								or: [each parent isCascade and: [each parent messages last = each]])]]
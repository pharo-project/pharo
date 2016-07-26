tests - ok
testBlockRewritesAreNotChained
	"This test shows that rewrite rules are not chained sequentially. One is applied and this is it."

	| tree |
	tree := self treeToBeRewritten.	
	"asdf ->  fdsa but not fdsa -> grgrgrgrgr"
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | true ].
	rewriter replace: 'fdsa' with: 'grgrgrgrgr' when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(RBParser
				parseMethod:
					'method: asdf
	<primitive: 1>
	<primitive: 2>
	^fdsa +  self foo + fdsa')
tests - ok
testBlockRewritesFirstRuleTakePrecedence
	| tree |
	tree := self treeToBeRewritten.	
	"Here the rule says that we only replace in the rightmost children of the return node."
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | true ].
	rewriter replace: 'asdf' with: 'grgrgrgrgr' when: [ :aNode | true ].
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
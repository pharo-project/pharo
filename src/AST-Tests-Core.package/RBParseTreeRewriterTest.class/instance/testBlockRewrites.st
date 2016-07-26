tests - ok
testBlockRewrites
	"This test shows that several rules can be used to specify different rewrite actions: the location in the tree structure, simple expression (self foo) using the node of the expression itself, and depending on the node kinds."

	| tree |
	tree := self treeToBeRewritten.
	"Here the rule says that we only replace in the rightmost children of the return node."
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | aNode parent parent isReturn ].	
	"here we want the replace self foo by the value the selector of the self foo expression, i.e. foo"
	rewriter replace: 'self foo' withValueFrom: [ :aNode | RBVariableNode named: aNode selector asString ].	
	"here the condition is false so the rule is not executed."
	rewriter replaceArgument: 'asdf' withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ] when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(RBParser
				parseMethod:
					'method: xxx
	<primitive: 1>
	<primitive: 2>
	^asdf +  foo + fdsa')